//! An spc700 assembler, used to compile the audio driver

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![forbid(unsafe_code)]

use spc700asm::{assemble_loaded_file, load_asm_file_and_includes};

use anstream::{eprint, eprintln};
use anstyle::{AnsiColor, Color, Style};
use clap::Parser;
use std::path::{Path, PathBuf};

const RESET: anstyle::Reset = anstyle::Reset;
const ERROR: Style = Style::new()
    .bold()
    .fg_color(Some(Color::Ansi(AnsiColor::Red)));

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(after_help = "A basic spc700 assembler")]
#[command(arg_required_else_help = true)]
struct Args {
    #[arg(short = 'o', long, value_name = "BIN_FILE", help = "output binary")]
    output: PathBuf,

    #[arg(value_name = "source", help = "Assembly source file")]
    asm_file: PathBuf,

    #[arg(
        short = 'm',
        long = "mlb",
        value_name = "MLB_FILE",
        help = "Output a Mesen label file"
    )]
    mlb: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();

    let asm_file = match load_asm_file_and_includes(&args.asm_file) {
        Ok(f) => f,
        Err(e) => {
            eprint!("{}", e.color_display());
            std::process::exit(1);
        }
    };

    let c = match assemble_loaded_file(&asm_file) {
        Ok(c) => c,
        Err(e) => {
            eprint!("{}", e.color_display(&asm_file));
            std::process::exit(1);
        }
    };

    let symbol_file = args.mlb.map(|path| match c.sym_file.to_mlb_label_file() {
        Ok(mlb) => (path, mlb),
        Err(e) => {
            eprintln!("Error creating mlb file: {e}");
            std::process::exit(1);
        }
    });

    write_file(&args.output, &c.output);

    if let Some((path, sym_file)) = symbol_file {
        write_file(&path, sym_file.as_bytes());
    }
}

fn write_file(path: &Path, contents: &[u8]) {
    match std::fs::write(path, contents) {
        Ok(()) => (),
        Err(e) => {
            eprintln!(
                "{RESET}{ERROR}Error writing {}{RESET}: {}",
                path.display(),
                e
            );
            std::process::exit(1);
        }
    }
}
