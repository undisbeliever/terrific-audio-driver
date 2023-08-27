//! compiler binary

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use clap::{Args, Parser, Subcommand};
use compiler::data::{is_name_or_id, load_text_file_with_limit, TextFile};
use compiler::{build_pitch_table, compile_mml, song_data, UniqueNamesProjectFile};
use compiler::{sound_effects, Name};

use std::ffi::OsString;
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

    /// Export a MML song as a .spc file
    Song2spc(CompileSongDataArgs),
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

    #[arg(value_name = "JSON_FILE", help = "project json file")]
    json_file: PathBuf,
}

fn compile_sound_effects(
    pf: &UniqueNamesProjectFile,
) -> Result<sound_effects::CombinedSoundEffectsData, ()> {
    let sound_effects = match &pf.sound_effect_file {
        Some(path) => {
            let path = pf.parent_path.join(path);
            match sound_effects::load_sound_effects_file(&path) {
                Err(e) => {
                    eprintln!("{}", e);
                    return Err(());
                }
                Ok(sfx_file) => {
                    match sound_effects::compile_sound_effects_file(&sfx_file, &pf.instruments) {
                        Err(e) => {
                            eprintln!("{}", e.multiline_display());
                            return Err(());
                        }
                        Ok(v) => v,
                    }
                }
            }
        }
        None => {
            if pf.sound_effects.is_empty() {
                Vec::new()
            } else {
                eprintln!("No sound effect file in {}", pf.file_name);
                return Err(());
            }
        }
    };

    match sound_effects::combine_sound_effects(&sound_effects, pf) {
        Ok(sfx) => Ok(sfx),
        Err(e) => {
            eprintln!("Error compiling sound effects: {}", e);
            Err(())
        }
    }
}

fn compile_common_data(args: CompileCommonDataArgs) {
    let pf = load_project_file(&args.json_file);

    let samples = match compiler::build_sample_and_instrument_data(&pf) {
        Ok(samples) => Ok(samples),
        Err(e) => {
            eprintln!("{}", e.multiline_display());
            Err(())
        }
    };

    let sfx = compile_sound_effects(&pf);

    let (samples, sfx) = match (samples, sfx) {
        (Ok(samples), Ok(sfx)) => (samples, sfx),
        _ => error!("Error compiling common audio data"),
    };

    let cad = match compiler::build_common_audio_data(&samples, &sfx) {
        Ok(data) => data,
        Err(e) => error!("{}", e.multiline_display()),
    };

    write_data(args.output, cad.data());
}

//
// Compile Songs
// =============

#[derive(Args)]
struct CompileSongDataArgs {
    #[command(flatten)]
    output: OutputArg,

    #[arg(value_name = "JSON_FILE", help = "project json file")]
    json_file: PathBuf,

    #[arg(value_name = "SONG", help = "song name, song number, or MML file")]
    song: OsString,
}

fn load_mml_file(
    args: &CompileSongDataArgs,
    pf: &UniqueNamesProjectFile,
) -> (TextFile, Option<Name>) {
    let song_name_or_path = &args.song;

    // Safe, the `U+FFFD REPLACEMENT CHARACTER` is not a name character.
    let sn = song_name_or_path.to_string_lossy();

    let (path, song_name) = if is_name_or_id(&sn) {
        if pf.songs.is_empty() {
            error!("No songs in project file");
        }
        match sn.parse::<usize>() {
            Ok(song_id) => match pf.get_song_from_id(song_id) {
                Some(s) => (pf.parent_path.join(&s.source), Some(s.name.clone())),
                None => error!(
                    "Song number out of range ({} - {})",
                    UniqueNamesProjectFile::FIRST_SONG_ID,
                    pf.last_song_id()
                ),
            },
            Err(_) => match pf.songs.get(&sn) {
                Some(s) => (pf.parent_path.join(&s.source), Some(s.name.clone())),
                None => error!("Cannot find song: {}", sn),
            },
        }
    } else {
        (song_name_or_path.into(), None)
    };

    let text_file = load_text_file(path);

    (text_file, song_name)
}

fn compile_song_data(args: CompileSongDataArgs) {
    let pf = load_project_file(&args.json_file);
    let (mml_file, song_name) = load_mml_file(&args, &pf);

    let pitch_table = match build_pitch_table(&pf.instruments) {
        Ok(pt) => pt,
        Err(e) => error!("{}", e.multiline_display()),
    };

    let mml = match compile_mml(&mml_file, song_name, &pf.instruments, &pitch_table) {
        Ok(mml) => mml,
        Err(e) => error!("{}", e.multiline_display()),
    };

    let song_data = match song_data(mml) {
        Ok(d) => d,
        Err(e) => error!("{}", e),
    };

    write_data(args.output, song_data.data());
}

//
// Export song to .spc file
// ========================

fn export_song_to_spc_file(args: CompileSongDataArgs) {
    let pf = load_project_file(&args.json_file);
    let (mml_file, song_name) = load_mml_file(&args, &pf);

    let samples = match compiler::build_sample_and_instrument_data(&pf) {
        Ok(s) => s,
        Err(e) => error!("{}", e.multiline_display()),
    };
    let sfx = sound_effects::blank_compiled_sound_effects();

    let mml = match compile_mml(&mml_file, song_name, &pf.instruments, samples.pitch_table()) {
        Ok(mml) => mml,
        Err(e) => error!("{}", e.multiline_display()),
    };

    let song_data = match song_data(mml) {
        Ok(mml) => mml,
        Err(e) => error!("{}", e),
    };

    let common_audio_data = match compiler::build_common_audio_data(&samples, &sfx) {
        Ok(data) => data,
        Err(e) => error!("{}", e.multiline_display()),
    };

    let data = match compiler::export_spc_file(&common_audio_data, &song_data) {
        Ok(d) => d,
        Err(e) => error!("{}", e),
    };

    write_data(args.output, &data);
}

//
// Main
// ====

fn main() {
    let args = ArgParser::parse();

    match args.command {
        Command::Common(args) => compile_common_data(args),
        Command::Song(args) => compile_song_data(args),
        Command::Song2spc(args) => export_song_to_spc_file(args),
    }
}

//
// File functions
// ==============

fn load_project_file(path: &Path) -> UniqueNamesProjectFile {
    match compiler::load_project_file(path) {
        Err(e) => error!("Cannot load project file: {}", e),
        Ok(m) => match compiler::validate_project_file_names(m) {
            Ok(vm) => vm,
            Err(e) => error!("{}", e.multiline_display()),
        },
    }
}

fn load_text_file(path: PathBuf) -> TextFile {
    match load_text_file_with_limit(&path) {
        Ok(tf) => tf,
        Err(e) => error!("{}", e),
    }
}

fn write_data(out: OutputArg, data: &[u8]) {
    if let Some(path) = out.path {
        match fs::write(&path, data) {
            Ok(()) => (),
            Err(e) => error!("Error writing {}: {}", path.display(), e),
        }
    } else if out.stdout {
        match io::stdout().write_all(data) {
            Ok(()) => (),
            Err(e) => error!("Error writing data: {}", e),
        }
    }
}
