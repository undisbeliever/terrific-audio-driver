//! TAD loader structs/formats

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::driver_constants::BYTES_PER_LOADER_TRANSFER;

pub struct InterlacedRomData<'a>(&'a [u8]);

impl<'a> InterlacedRomData<'a> {
    pub(crate) fn new(data: &'a [u8]) -> Self {
        Self(data)
    }

    #[expect(
        clippy::len_without_is_empty,
        reason = "empty data has a ROM data length of 1"
    )]
    pub fn len(&self) -> usize {
        const _: () = assert!(BYTES_PER_LOADER_TRANSFER == 3);

        match self.0.len() {
            ..=1 => 1,
            l if l % 3 == 1 => l + 1,
            l => l,
        }
    }

    pub fn audio_ram_len(&self) -> usize {
        self.0.len().next_multiple_of(BYTES_PER_LOADER_TRANSFER)
    }

    pub fn to_vec(&self) -> Vec<u8> {
        self.iter().collect()
    }

    pub(crate) fn iter(&self) -> impl ExactSizeIterator<Item = u8> + '_ {
        const _: () = assert!(BYTES_PER_LOADER_TRANSFER == 3);

        let len = self.len();

        let partition = len.div_ceil(3);

        (0..len).map(move |i| {
            let p = i % 3;
            let d = i / 3;

            self.0.get(p * partition + d).copied().unwrap_or(0)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interlaced_data() {
        assert_eq!(InterlacedRomData(&[1]).to_vec(), &[1]);
        assert_eq!(InterlacedRomData(&[1, 2]).to_vec(), &[1, 2]);
        assert_eq!(InterlacedRomData(&[1, 2, 3]).to_vec(), &[1, 2, 3]);
        assert_eq!(InterlacedRomData(&[1, 2, 3, 4]).to_vec(), &[1, 3, 0, 2, 4]);
        assert_eq!(
            InterlacedRomData(&[1, 2, 3, 4, 5]).to_vec(),
            &[1, 3, 5, 2, 4]
        );
        assert_eq!(
            InterlacedRomData(&[1, 2, 3, 4, 5, 6]).to_vec(),
            &[1, 3, 5, 2, 4, 6]
        );
        assert_eq!(
            InterlacedRomData(&[1, 2, 3, 4, 5, 6, 7]).to_vec(),
            &[1, 4, 7, 2, 5, 0, 3, 6]
        );
        assert_eq!(
            InterlacedRomData(&[1, 2, 3, 4, 5, 6, 7, 8]).to_vec(),
            &[1, 4, 7, 2, 5, 8, 3, 6]
        );
        assert_eq!(
            InterlacedRomData(&[1, 2, 3, 4, 5, 6, 7, 8, 9]).to_vec(),
            &[1, 4, 7, 2, 5, 8, 3, 6, 9]
        );
    }
}
