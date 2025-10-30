//! Subroutine compiler manager

// SPDX-FileCopyrightText: © 2024 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::commands::{Command, CommandWithPos, SubroutineCommands};

use crate::driver_constants::MAX_SUBROUTINES;

const MISSING_SUBROUTINE_BIT: u8 = u8::MAX;

const _: () = assert!(MAX_SUBROUTINES <= MISSING_SUBROUTINE_BIT as usize);

struct SubroutineBitArray {
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

fn subroutine_call_bitset(commands: &[CommandWithPos]) -> SubroutineBitArray {
    let mut o = SubroutineBitArray::new_all_clear();

    for c in commands {
        if let Command::CallSubroutine(i, _) = c.command() {
            let bit = *i;
            debug_assert!(bit < MISSING_SUBROUTINE_BIT);
            o.set_bit(bit);
        }
    }

    o
}

pub struct SubroutineCommandsWithCompileOrder<'a> {
    subroutines: Vec<SubroutineCommands<'a>>,
    compile_order: Vec<u8>,
}

impl<'a> SubroutineCommandsWithCompileOrder<'a> {
    pub fn compile_iter(&self) -> impl Iterator<Item = &SubroutineCommands<'a>> {
        self.compile_order
            .iter()
            .map(|&i| &self.subroutines[usize::from(i)])
    }
}

struct ToProcess {
    index: u8,
    function_calls: SubroutineBitArray,
}

/// Determine the subroutine compile order
///
/// Uses a bit-array to ensure the subroutines are compiled bottom-up order to detect recursion
/// and ensure note_tracking is correct.
pub fn subroutine_compile_order<'a>(
    subroutines: Vec<SubroutineCommands<'a>>,
) -> SubroutineCommandsWithCompileOrder<'a> {
    assert!(subroutines.len() <= SubroutineBitArray::N_BITS);

    let mut compile_order = Vec::with_capacity(subroutines.len());

    let mut to_process: Vec<_> = subroutines
        .iter()
        .map(|s| ToProcess {
            index: s.index,
            function_calls: subroutine_call_bitset(&s.commands),
        })
        .collect();

    // A bit-array of the subroutines that have yet to be compiled.
    // ANDed with `ToProcess::function_calls` to determine if all function calls in the subroutine have been compiled.
    let mut remaining_subroutines = SubroutineBitArray::new_all_set();

    remaining_subroutines.clear_bit(MISSING_SUBROUTINE_BIT);

    while !to_process.is_empty() {
        let old_len = to_process.len();

        to_process.retain_mut(|s| {
            if s.function_calls
                .bitand_and_zero_test(&remaining_subroutines)
            {
                remaining_subroutines.clear_bit(s.index);

                compile_order.push(s.index);

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

    compile_order.extend(to_process.into_iter().map(|s| s.index));

    SubroutineCommandsWithCompileOrder {
        subroutines,
        compile_order,
    }
}
