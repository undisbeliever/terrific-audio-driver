//! compiler binary

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use clap::{Args, Parser, Subcommand};
use compiler::data::{is_name_or_id, load_text_file_with_limit, TextFile};
use compiler::{build_pitch_table, parse_mml, song_data, SoundEffectsFile, UniqueNamesProjectFile};

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

    #[arg(value_name = "TXT_FILE", help = "sound_effects txt file")]
    sfx_file: PathBuf,
}

fn compile_common_data(args: CompileCommonDataArgs) {
    let file_name = file_name(&args.sfx_file);

    let pf = load_project_file(&args.json_file);
    let sfx_file = load_sfx_file(args.sfx_file);

    let samples = compiler::build_sample_and_instrument_data(&pf);
    let sfx = compiler::compile_sound_effects_file(&sfx_file, &pf);

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

    #[arg(value_name = "JSON_FILE", help = "project json file")]
    json_file: PathBuf,

    #[arg(value_name = "SONG", help = "song name, song number, or MML file")]
    song: OsString,
}

fn load_mml_file(args: &CompileSongDataArgs, pf: &UniqueNamesProjectFile) -> TextFile {
    let song_name_or_path = &args.song;

    // Safe, the `U+FFFD REPLACEMENT CHARACTER` is not a name character.
    let sn = song_name_or_path.to_string_lossy();

    let path = if is_name_or_id(&sn) {
        if pf.songs.is_empty() {
            error!("No songs in project file");
        }
        match sn.parse::<usize>() {
            Ok(song_id) => match pf.get_song_from_id(song_id) {
                Some(s) => &s.source,
                None => error!(
                    "Song number out of range ({} - {})",
                    UniqueNamesProjectFile::FIRST_SONG_ID,
                    pf.last_song_id()
                ),
            },
            Err(_) => match pf.songs.get(&sn) {
                Some(s) => &s.source,
                None => error!("Cannot find song: {}", sn),
            },
        }
    } else {
        Path::new(song_name_or_path)
    };

    load_text_file(path.to_path_buf())
}

fn compile_song_data(args: CompileSongDataArgs) {
    let pf = load_project_file(&args.json_file);
    let mml_file = load_mml_file(&args, &pf);

    let mml_text = mml_file.contents;
    let file_name = mml_file.file_name;

    let pitch_table = match build_pitch_table(&pf.instruments) {
        Ok(pt) => pt,
        Err(e) => error!("{}", e.multiline_display()),
    };

    let mml = match parse_mml(&mml_text, &pf.instruments, &pitch_table) {
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
// Export song to .spc file
// ========================

fn export_song_to_spc_file(args: CompileSongDataArgs) {
    let pf = load_project_file(&args.json_file);
    let mml_file = load_mml_file(&args, &pf);

    let mml_text = mml_file.contents;
    let file_name = mml_file.file_name;

    let samples = match compiler::build_sample_and_instrument_data(&pf) {
        Ok(s) => s,
        Err(e) => error!("{}", e.multiline_display()),
    };
    let sfx = compiler::blank_compiled_sound_effects();

    let mml = match parse_mml(&mml_text, &pf.instruments, samples.pitch_table()) {
        Ok(mml) => mml,
        Err(e) => error!("{}", e.multiline_display(&file_name)),
    };

    let common_audio_data = match compiler::build_common_audio_data(&samples, &sfx) {
        Ok(data) => data,
        Err(e) => error!("{}", e.multiline_display()),
    };

    let data = match compiler::export_spc_file(&common_audio_data, &mml) {
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
        Command::Song2spc(args) => export_song_to_spc_file(args),
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

fn load_project_file(path: &Path) -> UniqueNamesProjectFile {
    match compiler::load_project_file(path) {
        Err(e) => error!("Cannot load project file: {}", e),
        Ok(m) => match compiler::validate_project_file_names(m) {
            Ok(vm) => vm,
            Err(e) => error!("{}", e.multiline_display()),
        },
    }
}

fn load_sfx_file(path: PathBuf) -> SoundEffectsFile {
    match compiler::load_sound_effects_file(&path) {
        Ok(f) => f,
        Err(e) => error!("Cannot load sound effect file: {}", e),
    }
}

fn load_text_file(path: PathBuf) -> TextFile {
    match load_text_file_with_limit(&path) {
        Ok(tf) => tf,
        Err(e) => error!("{}", e),
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
