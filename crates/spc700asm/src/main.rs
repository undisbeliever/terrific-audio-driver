//! An spc700 assembler, used to compile the audio driver

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![forbid(unsafe_code)]

use spc700asm::assemble;

use anstream::{eprint, eprintln};
use anstyle::{AnsiColor, Color, Style};
use clap::Parser;
use std::path::PathBuf;

const RESET: anstyle::Reset = anstyle::Reset;
const ERROR: Style = Style::new()
    .bold()
    .fg_color(Some(Color::Ansi(AnsiColor::Red)));

macro_rules! error {
    ($($arg:tt)*) => {{
        eprintln!($($arg)*);
        std::process::exit(1);
    }};
}

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(after_help = "A basic spc700 assembler")]
#[command(arg_required_else_help = true)]
struct Args {
    #[arg(short = 'o', long, value_name = "BIN_FILE", help = "output binary")]
    output: PathBuf,

    #[arg(value_name = "source", help = "Assembly source file")]
    asm_file: PathBuf,
}

fn main() {
    let args = Args::parse();

    let asm_file = match std::fs::read_to_string(&args.asm_file) {
        Ok(f) => f,
        Err(e) => error!(
            "{RESET}{ERROR}Cannot load {}{RESET}: {}",
            args.asm_file.display(),
            e
        ),
    };

    let c = match assemble(&asm_file) {
        Ok(c) => c,
        Err(e) => {
            eprint!("{}", e.color_display(&args.asm_file));
            std::process::exit(1);
        }
    };

    match std::fs::write(&args.output, &c.output) {
        Ok(()) => (),
        Err(why) => error!(
            "{RESET}{ERROR}Error writing {}{RESET}: {}",
            args.output.display(),
            why
        ),
    }
}
