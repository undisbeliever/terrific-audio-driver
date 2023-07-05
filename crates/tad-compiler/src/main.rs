//! compiler binary

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use clap::{Args, Parser, Subcommand};
use compiler::{
    build_pitch_table, parse_mml, song_data, SoundEffectsFile, UniqueNamesMappingsFile,
};

use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

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

    /// Compile MML song
    Song(CompileSongDataArgs),
}

#[derive(Args)]
#[group(required = true, multiple = false)]
struct OutputArg {
    #[arg(
        short = 'o',
        long = "output",
        value_name = "FILE",
        help = "output file"
    )]
    path: Option<PathBuf>,

    #[arg(long, help = "Write to stdout")]
    stdout: bool,
}

// Compile Common Audio Data
// =========================

#[derive(Args)]
struct CompileCommonDataArgs {
    #[command(flatten)]
    output: OutputArg,

    #[arg(value_name = "JSON_FILE", help = "instruments and mappings json file")]
    json_file: PathBuf,

    #[arg(value_name = "TXT_FILE", help = "sound_effects txt file")]
    sfx_file: PathBuf,
}

fn compile_common_data(args: CompileCommonDataArgs) {
    let file_name = file_name(&args.sfx_file);

    let mappings = load_mappings_file(args.json_file);
    let sfx_file = load_sfx_file(args.sfx_file);

    let samples = compiler::build_sample_and_instrument_data(&mappings);
    let sfx = compiler::compile_sound_effects_file(&sfx_file, &mappings);

    let (samples, sfx) = match (samples, sfx) {
        (Ok(samples), Ok(sfx)) => (samples, sfx),

        (Err(e), Ok(_)) => error!("{}", e.multiline_display()),
        (Ok(_), Err(e)) => error!("{}", e.multiline_display(&file_name)),
        (Err(e1), Err(e2)) => {
            eprintln!("{}", e1.multiline_display());
            error!("{}", e2.multiline_display(&file_name));
        }
    };

    let data = match compiler::build_common_audio_data(&samples, &sfx) {
        Ok(data) => data,
        Err(e) => error!("{}", e.multiline_display()),
    };

    write_data(args.output, data);
}

//
// Compile Songs
// =============

#[derive(Args)]
struct CompileSongDataArgs {
    #[command(flatten)]
    output: OutputArg,

    #[arg(value_name = "JSON_FILE", help = "instruments and mappings json file")]
    json_file: PathBuf,

    #[arg(value_name = "MML_FILE", help = "mml song file")]
    mml_file: PathBuf,
}

fn compile_song_data(args: CompileSongDataArgs) {
    let file_name = file_name(&args.mml_file);

    let mml_text = load_mml_file(args.mml_file);
    let mappings = load_mappings_file(args.json_file);

    let pitch_table = match build_pitch_table(&mappings.instruments) {
        Ok(pt) => pt,
        Err(e) => error!("{}", e.multiline_display()),
    };

    let mml = match parse_mml(&mml_text, &mappings.instruments, &pitch_table) {
        Ok(mml) => mml,
        Err(e) => error!("{}", e.multiline_display(&file_name)),
    };

    let data = match song_data(&mml) {
        Ok(d) => d,
        Err(e) => error!("{}", e),
    };

    write_data(args.output, data);
}

//
// Main
// ====

fn main() {
    let args = ArgParser::parse();

    match args.command {
        Command::Common(args) => compile_common_data(args),
        Command::Song(args) => compile_song_data(args),
    }
}

//
// File functions
// ==============

fn file_name(path: &Path) -> String {
    path.file_name()
        .unwrap_or(path.as_os_str())
        .to_string_lossy()
        .to_string()
}

fn load_mappings_file(path: PathBuf) -> UniqueNamesMappingsFile {
    match compiler::load_mappings_file(path) {
        Err(e) => error!("Cannot load mappings file: {}", e),
        Ok(m) => match compiler::validate_mappings_file_names(m) {
            Ok(vm) => vm,
            Err(e) => error!("Invalid mappings File: {}", e),
        },
    }
}

fn load_sfx_file(path: PathBuf) -> SoundEffectsFile {
    let contents = match fs::read_to_string(&path) {
        Ok(s) => s,
        Err(e) => error!("Cannot load sound effect file: {}", e),
    };

    compiler::sfx_file_from_string(contents, &path)
}

fn load_mml_file(path: PathBuf) -> String {
    match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => error!("Cannot load mml file: {}", e),
    }
}

fn write_data(out: OutputArg, data: Vec<u8>) {
    if let Some(path) = out.path {
        match fs::write(&path, data) {
            Ok(()) => (),
            Err(e) => error!("Error writing {}: {}", path.display(), e),
        }
    } else if out.stdout {
        match io::stdout().write_all(&data) {
            Ok(()) => (),
            Err(e) => error!("Error writing data: {}", e),
        }
    }
}
