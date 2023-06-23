//! compiler binary

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use clap::{Args, Parser, Subcommand};
use compiler::{MappingsFile, SoundEffectsFile};

use std::fs;
use std::path::PathBuf;

macro_rules! error {
    ($($arg:tt)*) => {{
        eprintln!($($arg)*);
        std::process::exit(1);
    }};
}

#[derive(Parser)]
#[command(author, version)]
#[command(about = "toname audio driver compiler")] // ::TODO rename this project or change this value::
#[command(arg_required_else_help = true)]
struct ArgParser {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Compile common audio data
    Common(CompileCommonDataArgs),
}

// Compile Common Audio Data
// =========================

#[derive(Args)]
struct CompileCommonDataArgs {
    #[arg(short = 'o', long, value_name = "FILE", help = "output file")]
    output: PathBuf,

    #[arg(value_name = "JSON_FILE", help = "instruments and mappings json file")]
    json_file: PathBuf,

    #[arg(value_name = "TXT_FILE", help = "sound_effects txt file")]
    sfx_file: PathBuf,
}

fn compile_common_data(args: CompileCommonDataArgs) {
    let mappings = load_mappings_file(args.json_file);
    let sfx_file = load_sfx_file(args.sfx_file);

    let data = match compiler::compile_common_audio_data(&mappings, &sfx_file) {
        Ok(data) => data,
        Err(errors) => {
            eprintln!("Cannot compile common audio data");
            for e in errors {
                eprintln!("{}", e);
            }
            std::process::exit(1);
        }
    };

    write_data(args.output, data);
}

fn main() {
    let args = ArgParser::parse();

    match args.command {
        Command::Common(c) => compile_common_data(c),
    }
}

fn load_mappings_file(path: PathBuf) -> MappingsFile {
    match compiler::load_mappings_file(path) {
        Ok(m) => m,
        Err(e) => error!("{}", e),
    }
}

fn load_sfx_file(path: PathBuf) -> SoundEffectsFile {
    let contents = match fs::read_to_string(&path) {
        Ok(s) => s,
        Err(e) => error!("Cannot load sound effect file: {}", e),
    };

    compiler::sfx_file_from_string(contents, &path)
}

fn write_data(path: PathBuf, data: Vec<u8>) {
    match fs::write(&path, data) {
        Ok(()) => (),
        Err(why) => error!("Error writing {}: {}", path.display(), why),
    }
}
