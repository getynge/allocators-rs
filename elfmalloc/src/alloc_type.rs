// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Global tags to indicate the allocation subsystem from which a pointer originates.
//!
//! The `AllocType` enum is used to mark pages in elfmalloc; see the `general` and `slag` modules
//! for more on how this works.

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum AllocType {
    SmallSlag,
    BigSlag,
    Large,
}

const NONCE_SMALL_SLAG: usize = 3425969530822445019;
const NONCE_BIG_SLAG: usize = 6024217769238108171;
const NONCE_LARGE: usize = 7936710879568072041;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct AllocTypeContainer(usize);

impl AllocTypeContainer {
    pub fn new(typ: AllocType) -> AllocTypeContainer {
        AllocTypeContainer(match typ {
            AllocType::SmallSlag => NONCE_SMALL_SLAG,
            AllocType::BigSlag => NONCE_BIG_SLAG,
            AllocType::Large => NONCE_LARGE,
        })
    }

    pub fn alloc_type(&self) -> Option<AllocType> {
        use self::AllocType::*;
        if self.0 == NONCE_SMALL_SLAG {
            Some(SmallSlag)
        } else if self.0 == NONCE_BIG_SLAG {
            Some(BigSlag)
        } else if self.0 == NONCE_LARGE {
            Some(Large)
        } else {
            None
        }
    }
}
