// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Allocator-safe thread-local storage.
//!
//! The `tls` module implements thread-local storage that, unlike the standard library's
//! implementation, is safe for use in a global allocator.

#![feature(const_fn)]
#![feature(const_unsafe_cell_new)]
#![feature(core_intrinsics)]
#![feature(fn_must_use)]

use std::cell::UnsafeCell;
use std::mem;

/// Declare a thread-local variable.
///
/// `alloc_thread_local` declares a thread-local variable which is safe for use in implementing a
/// global allocator. It is invoked as:
///
/// ```rust,ignore
/// alloc_thread_local!{ static <name>: <type> = <expr>; }
/// ```
///
/// For example,
///
/// ```rust,ignore
/// alloc_thread_local!{ static FOO: usize = 0; }
/// ```
///
/// Thread-local variables follow a distinct lifecycle, and can be in one of four states:
/// - All thread-local variables start out as *uninitialized*.
/// - When a thread-local variable is first accessed, it is moved into the *initializing* state,
///   and its initializer is called.
/// - Once the initializer returns, the thread-local variable is initialized to the returned value,
///   and it moves into the *initialized* state.
/// - When the thread exits, the variable moves into the *dropped* state, and the variable is
///   dropped.
///
/// Thread-local variables can be accessed using the `with` method. If the variable is in the
/// *uninitialized* or *initialized* states, the variable can be accessed. Otherwise, it cannot,
/// and it is the caller's responsibility to figure out a workaround for its task that does not
/// involve accessing the thread-local variable.
#[macro_export]
macro_rules! alloc_thread_local {
    (static $name:ident: $t: ty = $init:expr;) => (
        static $name: $crate::TLSSlot<$t> = {
            fn __init() -> $t { $init }

            fn __drop() { $name.drop(); }

            thread_local!{ static DROPPER: $crate::CallOnDrop = $crate::CallOnDrop(__drop); }

            // DROPPER will only be dropped if it is first initialized, so we provide this function
            // to be called when the TLSSlot is first initialized. The act of calling DROPPER.with
            // will cause DROPPER to be initialized, ensuring that it will later be dropped on
            // thread exit.
            fn __register_dtor() { DROPPER.with(|_| {}); }

            $crate::TLSSlot::new(__init, __register_dtor)
        };
    )
}

#[derive(Eq, PartialEq)]
enum TLSValue<T> {
    Uninitialized,
    Initializing,
    Initialized(T),
    Dropped,
}

#[derive(PartialEq, Eq, Debug)]
enum TLSState {
    Uninitialized,
    Initializing,
    Initialized,
    Dropped,
}

impl<T> TLSValue<T> {
    fn state(&self) -> TLSState {
        match self {
            &TLSValue::Uninitialized => TLSState::Uninitialized,
            &TLSValue::Initializing => TLSState::Initializing,
            &TLSValue::Initialized(_) => TLSState::Initialized,
            &TLSValue::Dropped => TLSState::Dropped,
        }
    }
}

/// A slot for a thread-local variable.
///
/// A `TLSSlot` should be initialized using the `internal_thread_local!` macro. See its
/// documentation for details on declaring and using thread-local variables.
pub struct TLSSlot<T> {
    slot: UnsafeCell<TLSValue<T>>,
    init: fn() -> T,
    register_dtor: fn(),
}

impl<T> TLSSlot<T> {
    #[doc(hidden)]
    pub const fn new(init: fn() -> T, register_dtor: fn()) -> TLSSlot<T> {
        TLSSlot {
            slot: UnsafeCell::new(TLSValue::Uninitialized),
            init,
            register_dtor,
        }
    }

    /// Access the TLS slot.
    ///
    /// `with` accepts a function that will be called with a reference to the TLS value. If the
    /// slot is in the *initializing* or *dropped* state, `with` will return `None` without
    /// invoking `f`. If the slot is in the *uninitialized* state, `with` will initialize the value
    /// and then call `f`. If the slot is in the *initialized* state, `with` will call `f`. In
    /// either of these last two cases, `with` will return `Some(r)`, where `r` is the value
    /// returned from the call to `f`.
    pub unsafe fn with<R, F: FnOnce(&mut T) -> R>(&self, f: F) -> Option<R> {
        use std::intrinsics::likely;
        if !likely(dyld_loaded()) {
            return None;
        }

        let ptr = self.slot.get();
        match &mut *ptr {
            &mut TLSValue::Initialized(ref mut t) => return Some(f(t)),
            &mut TLSValue::Uninitialized => {}
            &mut TLSValue::Initializing | &mut TLSValue::Dropped => return None,
        }

        // Move into to the Initializing state before registering the destructor in case
        // registering the destructor involves allocation. If it does, the nested access to this
        // TLS value will detect that the value is in state Initializing, the call to with will
        // return None, and a fallback path can be taken.
        *ptr = TLSValue::Initializing;
        *ptr = TLSValue::Initialized((self.init)());
        (self.register_dtor)();
        self.with(f)
    }

    #[doc(hidden)]
    pub fn drop(&self) {
        unsafe {
            let state = (&*self.slot.get()).state();
            // if state != TLSState::Initialized {
            //     eprintln!("TLSValue dropped while in state {:?}", state);
            //     eprintln!("current stack:");
            //     backtrace::trace(|frame| {
            //         let ip = frame.ip();
            //         backtrace::resolve(ip, |symbol| {
            //             if let Some(name) = symbol.name() {
            //                 eprintln!("{}", name);
            //             } else {
            //                 eprintln!("<unknown function>");
            //             }
            //             if let Some(path) = symbol.filename() {
            //                 if let Some(s) = path.to_str() {
            //                     eprint!("\t{}", s);
            //                 } else {
            //                     eprint!("\t<unknown file>");
            //                 }
            //             } else {
            //                 eprint!("\t<unknown file>");
            //             }
            //             if let Some(line) = symbol.lineno() {
            //                 eprintln!(":{}", line);
            //             } else {
            //                 eprintln!();
            //             }
            //         });
            //         true
            //     });
            // }
            assert!(
                state == TLSState::Initialized,
                "TLSValue dropped while in state {:?}",
                state
            );

            // According to a comment in the standard library, "The macOS implementation of TLS
            // apparently had an odd aspect to it where the pointer we have may be overwritten
            // while this destructor is running. Specifically if a TLS destructor re-accesses TLS
            // it may trigger a re-initialization of all TLS variables, paving over at least some
            // destroyed ones with initial values. This means that if we drop a TLS value in place
            // on macOS that we could revert the value to its original state halfway through the
            // destructor, which would be bad!" -
            // https://github.com/rust-lang/rust/blob/master/src/libstd/sys/unix/fast_thread_local.rs
            //
            // Thus, it's important that we use mem::replace here. That way, the value is brought
            // into tmp and then dropped while it is a local variable, avoiding this problem.
            let tmp = mem::replace(&mut *self.slot.get(), TLSValue::Dropped);
            mem::drop(tmp);
        }
    }
}

unsafe impl<T> Sync for TLSSlot<T> {}

// The mechanics of registering destructors is complicated and involves a lot of cross-platform
// logic. Instead of implementing that all ourselves, we piggy back on the standard library's
// TLS implementation. Each TLSSlot has a corresponding LocalKey (from the standard library) whose
// value is a CallOnDrop holding a function which will invoke the drop method on the TLSSlot. This
// function is called in CallOnDrop's Drop implementation.
#[doc(hidden)]
pub struct CallOnDrop(pub fn());

impl Drop for CallOnDrop {
    fn drop(&mut self) {
        (self.0)();
    }
}

// TODO: Modify this comment to include links to relevant docs/issues

// On Mac, TLS cannot be accessed while a dynamic library is being loaded (at least, that's what it
// appears from our own experimentation with DYLD_INSERT_LIBRARIES). Unfortunately, the code is
// used to load dynamic libraries performs allocations. Thus, when producing a Mac dynamic library
// (.dylib), we need to be able to detect whether we're being called from the loader itself. We
// accomplish this by using a global static (DYLD_LOADED) that indicates whether we've been loaded,
// and setting it to true in a library constructor (dyld_init).

#[cfg(all(feature = "dylib", target_os = "macos"))]
static mut DYLD_LOADED: bool = false;

fn dyld_loaded() -> bool {
    #[cfg(all(feature = "dylib", target_os = "macos"))]
    unsafe { DYLD_LOADED }
    #[cfg(not(all(feature = "dylib", target_os = "macos")))]
    true
}

// On Mac, the C ABI prefixes all symbols with _, so use the symbol name _dyld_init instead of
// dyld_init. Source: https://users.rust-lang.org/t/ld-preload-init-function-in-rust/12865/6
// TODO: Consider switching to using the .mod_init_funcs (e.g.,
// #[link_secction = ".mod_init_funcs"]) as recommended here:
// https://community.embarcadero.com/blogs/entry/mac-os-x-shared-library-initialization-5639

// TODO: #[must_use] doesn't seem to work here. Is there a way we can ensure compilation or link
// failure if dyld_init isn't linked as the constructor (or at least isn't used in some way)?

/// Dynamic load initializer.
///
/// While compiling a dynamic library on Mac, this function must be registered as a library
/// constructor. The top-level crate must include the following linker directive:
/// `#![cfg(link_args = "-Wl,-init,_dyld_init")]`.
///
/// Alternatively, if a library constructor is already used, place a call to this function as the
/// first line of that constructor.
#[cfg(all(feature = "dylib", target_os = "macos"))]
#[must_use]
#[no_mangle]
pub extern "C" fn dyld_init() {
    eprintln!("alloc-tls: dyld loaded");
    unsafe {
        DYLD_LOADED = true;
    }
}

#[cfg(test)]
mod tests {
    // Modified from the Rust standard library
    use std::sync::mpsc::{channel, Sender};
    use std::cell::UnsafeCell;
    use std::thread;

    struct Foo(Sender<()>);

    impl Drop for Foo {
        fn drop(&mut self) {
            let Foo(ref s) = *self;
            s.send(()).unwrap();
        }
    }

    #[test]
    fn smoke_dtor() {
        alloc_thread_local!{ static FOO: UnsafeCell<Option<Foo>> = UnsafeCell::new(None); }

        let (tx, rx) = channel();
        let _t = thread::spawn(move || unsafe {
            let mut tx = Some(tx);
            FOO.with(|f| {
                *f.get() = Some(Foo(tx.take().unwrap()));
            });
        });
        rx.recv().unwrap();
    }

    #[test]
    fn drop() {
        static mut DROPPED: bool = false;
        use super::CallOnDrop;
        fn drop() {
            unsafe { DROPPED = true }
        }
        alloc_thread_local!{ static FOO: CallOnDrop = CallOnDrop(drop); }

        thread::spawn(|| unsafe { FOO.with(|_| {}).unwrap() })
            .join()
            .unwrap();
        assert_eq!(unsafe { DROPPED }, true);
    }
}
