//! MML subroutine compiler manager

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::collections::HashMap;

use crate::driver_constants::MAX_SUBROUTINES;
use crate::errors::MmlChannelError;

use super::bc_generator::MmlSongBytecodeGenerator;
use super::tokenizer::{MmlTokens, Token};
use super::IdentifierStr;

const MISSING_SUBROUTINE_BIT: u8 = u8::MAX;

const _: () = assert!(MAX_SUBROUTINES <= MISSING_SUBROUTINE_BIT as usize);

pub struct SubroutineBitArray {
    bits: [u128; Self::N_BITS / Self::BITS_PER_ENTRY],
}

impl SubroutineBitArray {
    const N_BITS: usize = 256;
    const BITS_PER_ENTRY: usize = 128;

    pub fn new_all_clear() -> Self {
        Self {
            bits: Default::default(),
        }
    }

    pub fn new_all_set() -> Self {
        Self {
            bits: [u128::MAX; Self::N_BITS / Self::BITS_PER_ENTRY],
        }
    }

    pub fn set_bit(&mut self, bit: u8) {
        let bit = usize::from(bit);
        self.bits[bit / Self::BITS_PER_ENTRY] |= 1 << (bit % Self::BITS_PER_ENTRY);
    }

    pub fn clear_bit(&mut self, bit: u8) {
        let bit = usize::from(bit);
        self.bits[bit / Self::BITS_PER_ENTRY] &= !(1 << (bit % Self::BITS_PER_ENTRY));
    }

    /// Returns (self & o) == zero
    pub fn bitand_and_zero_test(&self, o: &SubroutineBitArray) -> bool {
        self.bits.iter().zip(o.bits.iter()).all(|(a, b)| a & b == 0)
    }
}

fn subroutine_call_bitset(
    tokens: &MmlTokens,
    subroutine_name_map: &HashMap<IdentifierStr, usize>,
) -> SubroutineBitArray {
    let mut o = SubroutineBitArray::new_all_clear();

    for t in tokens.token_iter() {
        if let Token::CallSubroutine(id) = t {
            match subroutine_name_map.get(id) {
                Some(&i) => {
                    let bit = i.try_into().unwrap();
                    debug_assert!(bit < MISSING_SUBROUTINE_BIT);
                    o.set_bit(bit);
                }
                None => {
                    o.set_bit(MISSING_SUBROUTINE_BIT);
                }
            }
        }
    }

    o
}

struct ToProcess<'a> {
    index: u8,
    identifier: IdentifierStr<'a>,
    tokens: MmlTokens<'a>,
    function_calls: SubroutineBitArray,
}

/// Compiles MML song subroutines
///
/// Uses a bit-array to ensure the subroutines are compiled bottom-up order to detect recursion
/// and ensure note_tracking is correct.
pub fn compile_subroutines<'a>(
    bc_generator: &mut MmlSongBytecodeGenerator<'a>,
    subroutines: Vec<(IdentifierStr<'a>, MmlTokens<'a>)>,
    subroutine_name_map: &HashMap<IdentifierStr<'a>, usize>,
) -> Vec<MmlChannelError> {
    assert!(subroutines.len() <= SubroutineBitArray::N_BITS);

    let mut to_process: Vec<_> = subroutines
        .into_iter()
        .enumerate()
        .map(|(i, (id, tokens))| ToProcess {
            function_calls: subroutine_call_bitset(&tokens, subroutine_name_map),
            index: i.try_into().unwrap(),
            identifier: id,
            tokens,
        })
        .collect();

    // A bit-array of the subroutines that have yet to be compiled.
    // ANDed with `ToProcess::function_calls` to determine if all function calls in the subroutine have been compiled.
    let mut remaining_subroutines = SubroutineBitArray::new_all_set();

    remaining_subroutines.clear_bit(MISSING_SUBROUTINE_BIT);

    let mut errors = Vec::with_capacity(to_process.len());

    while !to_process.is_empty() {
        let old_len = to_process.len();

        to_process.retain_mut(|s| {
            if s.function_calls
                .bitand_and_zero_test(&remaining_subroutines)
            {
                remaining_subroutines.clear_bit(s.index);

                let mut tokens = Default::default();
                std::mem::swap(&mut tokens, &mut s.tokens);

                let r = bc_generator.parse_and_compile_song_subroutione(s.identifier, tokens);
                if let Err(e) = r {
                    errors.push(e);
                }

                false
            } else {
                // keep s
                true
            }
        });

        // Break if there is recursion
        if to_process.len() == old_len {
            break;
        }
    }

    for s in to_process {
        let r = bc_generator.parse_and_compile_song_subroutione(s.identifier, s.tokens);
        if let Err(e) = r {
            errors.push(e);
        }
    }

    errors
}
