//! Mesen MLB label files

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use std::{fmt::Write, ops::RangeInclusive};

struct Symbol {
    full_name: String,
    addr: RangeInclusive<i64>,
}

pub struct SymbolFile {
    symbols: Vec<Symbol>,
}

impl SymbolFile {
    pub(crate) fn new() -> Self {
        Self {
            symbols: Vec::new(),
        }
    }

    pub(crate) fn add_var(&mut self, name: &str, addr: u16, size: u16) {
        let addr = i64::from(addr);
        let size = i64::from(size);

        match size {
            ..1 => panic!("Invalid size"),
            1.. => self.symbols.push(Symbol {
                full_name: name.to_owned(),
                addr: addr..=(addr + size - 1),
            }),
        }
    }

    pub(crate) fn add_label(&mut self, full_name: String, addr: i64) {
        self.symbols.push(Symbol {
            full_name,
            addr: addr..=addr,
        })
    }

    pub fn find(&self, name: &str) -> Option<i64> {
        self.symbols
            .iter()
            .find(|s| s.full_name == name)
            .map(|s| *s.addr.start())
    }

    pub fn to_mlb_label_file(&self) -> Result<String, std::fmt::Error> {
        let mut out = String::with_capacity(32 * 1024);

        for s in &self.symbols {
            // Mesen cannot import labels containing `.`.
            let label = s.full_name.replace(".", "_");

            if *s.addr.start() >= 0 && *s.addr.end() >= 0 {
                if s.addr.start() == s.addr.end() {
                    writeln!(out, "SpcRam:{:04X}:{}", s.addr.start(), label)?;
                } else {
                    writeln!(
                        out,
                        "SpcRam:{:04X}-{:04X}:{}",
                        s.addr.start(),
                        s.addr.end(),
                        label
                    )?;
                }
            }
        }

        Ok(out)
    }
}
