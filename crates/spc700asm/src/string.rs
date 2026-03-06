//! String utilities

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

pub fn split_first_word(s: &str) -> (&str, &str) {
    match s.split_once(|c: char| c.is_ascii_whitespace()) {
        Some((a, b)) => (a, b.trim_start()),
        None => (s, ""),
    }
}

pub struct CommaIter<'a>(&'a str);

impl<'a> Iterator for CommaIter<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        let s = self.0.trim_start();

        if !s.is_empty() {
            let mut in_quote = false;
            let mut in_str = false;

            for (i, c) in s.bytes().enumerate() {
                match c {
                    b',' if !in_quote && !in_str => {
                        self.0 = &s[i + 1..];
                        return Some(s[..i].trim_end());
                    }
                    b'\'' if !in_str => in_quote = !in_quote,
                    b'\"' if !in_quote => in_str = !in_str,
                    _ => (),
                }
            }

            self.0 = "";
            Some(s.trim_end())
        } else {
            None
        }
    }
}

pub fn comma_iter(s: &str) -> CommaIter<'_> {
    CommaIter(s)
}

#[cfg(test)]
mod tests {
    #[test]
    fn comma_iter() {
        let mut it = super::comma_iter("one, two, three");
        assert_eq!(it.next(), Some("one"));
        assert_eq!(it.next(), Some("two"));
        assert_eq!(it.next(), Some("three"));
        assert_eq!(it.next(), None);

        let mut it = super::comma_iter("one, ',', three");
        assert_eq!(it.next(), Some("one"));
        assert_eq!(it.next(), Some("','"));
        assert_eq!(it.next(), Some("three"));
        assert_eq!(it.next(), None);

        let mut it = super::comma_iter(r##"one, ", two, three"  , four, five"##);
        assert_eq!(it.next(), Some("one"));
        assert_eq!(it.next(), Some(r##"", two, three""##));
        assert_eq!(it.next(), Some("four"));
        assert_eq!(it.next(), Some("five"));
        assert_eq!(it.next(), None);
    }
}
