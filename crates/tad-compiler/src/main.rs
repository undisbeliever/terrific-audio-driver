//! compiler binary

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use clap::{Args, Parser, Subcommand};

use compiler::{
    common_audio_data::{build_common_audio_data, CommonAudioData},
    data::{
        is_name_or_id, load_text_file_with_limit, load_text_file_with_limit_path, Name, Song,
        TextFile, UniqueNamesProjectFile,
    },
    mml::compile_mml,
    mml_tick_count::build_tick_count_table,
    pitch_table::build_pitch_table,
    pitch_table::PitchTable,
    samples::build_sample_and_instrument_data,
    songs::{song_data, song_duration_string, validate_song_size, SongData},
    sound_effects,
    spc_file_export::export_spc_file,
};

use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};

macro_rules! error {
    ($($arg:tt)*) => {{
        eprintln!($($arg)*);
        std::process::exit(1);
    }};
}

#[derive(Parser)]
#[command(author, version)]
#[command(about = "terrific audio driver compiler")]
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

    /// Check the project will compile successfully and all songs fit in audio-RAM
    Check(CheckProjectArgs),
}

#[derive(Args)]
#[group(required = true, multiple = false)]
pub struct OutputArg {
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

    #[arg(value_name = "PROJECT_FILE", help = "project file")]
    project_file: PathBuf,
}

fn compile_sound_effects(
    pf: &UniqueNamesProjectFile,
) -> Result<sound_effects::CombinedSoundEffectsData, ()> {
    let sound_effects = match &pf.sound_effect_file {
        Some(sfx_file_source) => {
            match sound_effects::load_sound_effects_file(sfx_file_source, &pf.parent_path) {
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
    let output_arg = args.output.validate();

    let pf = load_project_file(&args.project_file);

    let samples = match build_sample_and_instrument_data(&pf) {
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

    let cad = match build_common_audio_data(&samples, &sfx) {
        Ok(data) => data,
        Err(e) => error!("{}", e.multiline_display()),
    };

    write_data(output_arg, cad.data());
}

//
// Compile Songs
// =============

#[derive(Args)]
#[group(required = false)]
struct SongOptions {
    #[arg(
        short = 't',
        long = "tick-count",
        help = "print tick count table",
        conflicts_with = "stdout"
    )]
    print_tick_counts: bool,
}

#[derive(Args)]
struct CompileSongDataArgs {
    #[command(flatten)]
    output: OutputArg,

    #[arg(value_name = "PROJECT_FILE", help = "project file")]
    project_file: PathBuf,

    #[arg(value_name = "SONG", help = "song name, song number, or MML file")]
    song: OsString,

    #[command(flatten)]
    options: SongOptions,
}

fn load_mml_file(
    song_name_or_path: &OsStr,
    pf: &UniqueNamesProjectFile,
) -> (TextFile, Option<Name>) {
    // Safe, the `U+FFFD REPLACEMENT CHARACTER` is not a name character.
    let sn = song_name_or_path.to_string_lossy();

    if is_name_or_id(&sn) {
        if pf.songs.is_empty() {
            error!("No songs in project file");
        }
        let (source, song_name) = match sn.parse::<usize>() {
            Ok(song_id) => match pf.get_song_from_id(song_id) {
                Some(s) => (s.source.clone(), Some(s.name.clone())),
                None => error!(
                    "Song number out of range ({} - {})",
                    UniqueNamesProjectFile::FIRST_SONG_ID,
                    pf.last_song_id()
                ),
            },
            Err(_) => match pf.songs.get(&sn) {
                Some(s) => (s.source.clone(), Some(s.name.clone())),
                None => error!("Cannot find song: {}", sn),
            },
        };
        let text_file = match load_text_file_with_limit(&source, &pf.parent_path) {
            Ok(tf) => tf,
            Err(e) => error!("{}", e),
        };
        (text_file, song_name)
    } else {
        let path = PathBuf::from(song_name_or_path);
        let text_file = match load_text_file_with_limit_path(&path) {
            Ok(tf) => tf,
            Err(e) => error!("{}", e),
        };
        (text_file, None)
    }
}

fn compile_song(
    mml_file: TextFile,
    song_name: Option<Name>,
    options: &SongOptions,
    pf: &UniqueNamesProjectFile,
    pitch_table: &PitchTable,
) -> SongData {
    let mml = match compile_mml(&mml_file, song_name, &pf.instruments, pitch_table) {
        Ok(mml) => mml,
        Err(e) => error!("{}", e.multiline_display()),
    };

    let tick_count_table = match options.print_tick_counts {
        true => Some(build_tick_count_table(&mml)),
        false => None,
    };

    let song_data = match song_data(mml) {
        Ok(d) => d,
        Err(e) => error!("{}", e),
    };

    if let Some(tct) = tick_count_table {
        println!("Duration: {}", song_duration_string(song_data.duration()));
        println!("{}", tct);
    }

    song_data
}

fn compile_song_data(args: CompileSongDataArgs) {
    let output_arg = args.output.validate();

    let pf = load_project_file(&args.project_file);
    let (mml_file, song_name) = load_mml_file(&args.song, &pf);

    let pitch_table = match build_pitch_table(&pf.instruments) {
        Ok(pt) => pt,
        Err(e) => error!("{}", e.multiline_display()),
    };
    let song_data = compile_song(mml_file, song_name, &args.options, &pf, &pitch_table);

    write_data(output_arg, song_data.data());
}

//
// Export song to .spc file
// ========================

fn export_song_to_spc_file(args: CompileSongDataArgs) {
    let output_arg = args.output.validate();

    let pf = load_project_file(&args.project_file);
    let (mml_file, song_name) = load_mml_file(&args.song, &pf);

    let samples = match build_sample_and_instrument_data(&pf) {
        Ok(s) => s,
        Err(e) => error!("{}", e.multiline_display()),
    };
    let sfx = sound_effects::blank_compiled_sound_effects();

    let song_data = compile_song(
        mml_file,
        song_name,
        &args.options,
        &pf,
        samples.pitch_table(),
    );

    let common_audio_data = match build_common_audio_data(&samples, &sfx) {
        Ok(data) => data,
        Err(e) => error!("{}", e.multiline_display()),
    };

    let data = match export_spc_file(&common_audio_data, &song_data) {
        Ok(d) => d,
        Err(e) => error!("{}", e),
    };

    write_data(output_arg, &data);
}

//
// Check project
// ==============

#[derive(Args)]
struct CheckProjectArgs {
    #[arg(value_name = "PROJECT_FILE", help = "project file")]
    project_file: PathBuf,
}

fn check_song(
    song: &Song,
    pf: &UniqueNamesProjectFile,
    pitch_table: &PitchTable,
    common_data: &CommonAudioData,
) -> Result<(), String> {
    let mml_file = match load_text_file_with_limit(&song.source, &pf.parent_path) {
        Ok(tf) => tf,
        Err(e) => return Err(format!("Error compiling {}: {}", song.name, e)),
    };

    let mml = match compile_mml(
        &mml_file,
        Some(song.name.clone()),
        &pf.instruments,
        pitch_table,
    ) {
        Ok(mml) => mml,
        Err(e) => return Err(e.multiline_display().to_string()),
    };

    let song_data = match song_data(mml) {
        Ok(sd) => sd,
        Err(e) => return Err(e.to_string()),
    };

    match validate_song_size(&song_data, common_data.data().len()) {
        Ok(()) => Ok(()),
        Err(e) => Err(format!(
            "Error compiling {}: {}",
            song.name,
            e.multiline_display()
        )),
    }
}

fn check_project(args: CheckProjectArgs) {
    let pf = load_project_file(&args.project_file);

    let samples = build_sample_and_instrument_data(&pf);
    if let Err(e) = samples {
        error!("{}", e.multiline_display())
    };

    let sfx = compile_sound_effects(&pf);

    let (samples, sfx) = match (samples, sfx) {
        (Ok(samples), Ok(sfx)) => (samples, sfx),
        _ => error!("Error compiling common audio data"),
    };

    let common_audio_data = match build_common_audio_data(&samples, &sfx) {
        Ok(data) => data,
        Err(e) => error!("{}", e.multiline_display()),
    };

    let mut n_song_errors = 0;

    for song in pf.songs.list().iter() {
        match check_song(song, &pf, samples.pitch_table(), &common_audio_data) {
            Ok(()) => (),
            Err(e) => {
                n_song_errors += 1;
                eprintln!("{}\n", e);
            }
        }
    }

    if n_song_errors == 0 {
        println!("Project is valid and will fit in audio-RAM");
    } else if n_song_errors == 1 {
        error!("{} song has an error", n_song_errors);
    } else {
        error!("{} songs have errors", n_song_errors);
    }
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
        Command::Check(args) => check_project(args),
    }
}

//
// File functions
// ==============

fn load_project_file(path: &Path) -> UniqueNamesProjectFile {
    match compiler::data::load_project_file(path) {
        Err(e) => error!("Cannot load project file: {}", e),
        Ok(m) => match compiler::data::validate_project_file_names(m) {
            Ok(vm) => vm,
            Err(e) => error!("{}", e.multiline_display()),
        },
    }
}

// Output
// ======

mod output {
    use crate::OutputArg;

    use std::fs;
    use std::io::{IsTerminal, Write};

    pub struct ValidatedOutputArg(OutputArg);

    pub fn validate_output_arg(out: OutputArg) -> ValidatedOutputArg {
        match (&out.path, out.stdout) {
            (Some(_), false) => ValidatedOutputArg(out),
            (None, true) => match std::io::stdout().is_terminal() {
                false => ValidatedOutputArg(out),
                true => error!("Error: --stdout will not write to a terminal."),
            },
            // This should not happen
            _ => panic!("Invalid output arguments"),
        }
    }

    pub fn write_data(out: ValidatedOutputArg, data: &[u8]) {
        if let Some(path) = &out.0.path {
            match fs::write(path, data) {
                Ok(()) => (),
                Err(e) => error!("Error writing {}: {}", path.display(), e),
            }
        } else if out.0.stdout {
            let mut stdout = std::io::stdout();

            if stdout.is_terminal() {
                error!("Cannot write binary data to stdout, stdout is a terminal");
            }

            match stdout.write_all(data) {
                Ok(()) => (),
                Err(e) => error!("Error writing data: {}", e),
            }
        } else {
            error!("Error writing data: No output");
        }
    }
}
use output::write_data;

impl OutputArg {
    fn validate(self) -> output::ValidatedOutputArg {
        output::validate_output_arg(self)
    }
}
