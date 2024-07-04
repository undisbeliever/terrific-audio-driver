//! compiler binary

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

#![forbid(unsafe_code)]

use clap::{Args, Parser, Subcommand};

use compiler::{
    common_audio_data::{build_common_audio_data, CommonAudioData},
    data::{
        is_name_or_id, load_text_file_with_limit, load_text_file_with_limit_path, Name, Song,
        TextFile, UniqueNamesProjectFile,
    },
    export::{
        bin_include_path, export_bin_file, Ca65Exporter, Ca65MemoryMap, ExportedBinFile, Exporter,
        MemoryMapMode, PvExporter, PvMemoryMap,
    },
    mml::{compile_mml, MmlTickCountTable},
    pitch_table::{build_pitch_table, PitchTable},
    samples::build_sample_and_instrument_data,
    sfx_file,
    songs::{song_duration_string, validate_song_size, SongData},
    sound_effects,
    spc_file_export::export_spc_file,
};

use std::collections::HashMap;
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
    Song2spc(Song2SpcArgs),

    /// Check the project will compile successfully and all songs fit in audio-RAM
    Check(CheckProjectArgs),

    /// Generate an ca65 include file containing songs and sound effect enums
    Ca65Enums(EnumArgs),

    /// Compile the project and output a ca65 assembly file containing LoadSongData and .incbin statements
    Ca65Export(Ca65ExportArgs),

    /// Generate an PVSnesLib include file containing songs and sound effect enums
    PvEnums(EnumArgs),

    /// Compile the project and output a PVSnesLib assembly file containing loadSongData() and .incbin statements
    PvExport(PvExportArgs),
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
    pitch_table: &PitchTable,
) -> Result<sound_effects::CombinedSoundEffectsData, ()> {
    let sound_effects = match &pf.sound_effect_file {
        Some(sfx_file_source) => {
            match sfx_file::load_sound_effects_file(sfx_file_source, &pf.parent_path) {
                Err(e) => {
                    eprintln!("{}", e);
                    return Err(());
                }
                Ok(sfx_file) => {
                    match sound_effects::compile_sound_effects_file(
                        &sfx_file,
                        &pf.instruments_and_samples,
                        pitch_table,
                    ) {
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
                HashMap::new()
            } else {
                eprintln!("No sound effect file in {}", pf.file_name);
                return Err(());
            }
        }
    };

    match sound_effects::combine_sound_effects(&sound_effects, pf.sound_effects.list()) {
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
        Ok(samples) => samples,
        Err(e) => {
            error!("{}", e.multiline_display());
        }
    };

    let sfx = match compile_sound_effects(&pf, samples.pitch_table()) {
        Ok(sfx) => sfx,
        Err(()) => error!("Error compiling sound effects"),
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
    let song_data = match compile_mml(
        &mml_file,
        song_name,
        &pf.instruments_and_samples,
        pitch_table,
    ) {
        Ok(mml) => mml,
        Err(e) => error!("{}", e.multiline_display()),
    };

    if options.print_tick_counts {
        println!("Duration: {}", song_duration_string(song_data.duration()));
        println!("{}", MmlTickCountTable(&song_data));
    }

    song_data
}

fn compile_song_data(args: CompileSongDataArgs) {
    let output_arg = args.output.validate();

    let pf = load_project_file(&args.project_file);
    let (mml_file, song_name) = load_mml_file(&args.song, &pf);

    let pitch_table = match build_pitch_table(&pf.instruments_and_samples) {
        Ok(pt) => pt,
        Err(e) => error!("{}", e.multiline_display()),
    };
    let song_data = compile_song(mml_file, song_name, &args.options, &pf, &pitch_table);

    write_data(output_arg, song_data.data());
}

//
// Export song to .spc file
// ========================

#[derive(Args)]
struct Song2SpcArgs {
    #[command(flatten)]
    song: CompileSongDataArgs,

    #[arg(
        short = 's',
        long = "sound-effects",
        help = "include sound effects in the .spc file"
    )]
    sound_effects: bool,
}

fn export_song_to_spc_file(args: Song2SpcArgs) {
    let output_arg = args.song.output.validate();

    let pf = load_project_file(&args.song.project_file);
    let (mml_file, song_name) = load_mml_file(&args.song.song, &pf);

    let samples = match build_sample_and_instrument_data(&pf) {
        Ok(s) => s,
        Err(e) => error!("{}", e.multiline_display()),
    };

    let sfx = if args.sound_effects {
        match compile_sound_effects(&pf, samples.pitch_table()) {
            Ok(sfx) => sfx,
            Err(()) => error!("Error compiling sound effects"),
        }
    } else {
        sound_effects::blank_compiled_sound_effects()
    };

    let song_data = compile_song(
        mml_file,
        song_name,
        &args.song.options,
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

fn check_project_command(args: CheckProjectArgs) {
    let pf = load_project_file(&args.project_file);
    compile_project(&pf);

    println!("Project is valid and will fit in audio-RAM");
}

//
// Enum Generators
// ===============

#[derive(Args)]
struct EnumArgs {
    #[command(flatten)]
    output: OutputArg,

    #[arg(value_name = "PROJECT_FILE", help = "project file")]
    project_file: PathBuf,
}

fn generate_enums_command<E: Exporter>(args: EnumArgs) {
    let output_arg = args.output.validate();

    let pf = load_project_file(&args.project_file);

    let inc_file = match E::generate_include_file(pf) {
        Ok(o) => o,
        Err(e) => error!("Error creating enum file: {}", e),
    };

    write_data(output_arg, inc_file.as_bytes());
}

//
// Export with assembly output
// ===========================

#[derive(Args)]
struct MemoryMapModeArgument {
    #[arg(
        long,
        conflicts_with = "hirom",
        group = "mm_mode",
        help = "LOROM mapping"
    )]
    lorom: bool,

    #[arg(
        long,
        conflicts_with = "lorom",
        group = "mm_mode",
        help = "HIROM mapping"
    )]
    hirom: bool,
}

impl MemoryMapModeArgument {
    fn mode(&self) -> MemoryMapMode {
        match (self.lorom, self.hirom) {
            (true, false) => MemoryMapMode::LoRom,
            (false, true) => MemoryMapMode::HiRom,
            (false, false) => error!("Missing --lorom or --hirom argument"),
            (true, true) => error!("Cannot use --lorom and --hirom arguments at the same time"),
        }
    }
}

#[derive(Args)]
struct ExportWithAsmArgs {
    #[arg(
        long,
        short = 'a',
        value_name = "ASM_FILE",
        help = "Assembly file output\n(contains LoadAudioData callback, .incbin statement and .assert statements)"
    )]
    output_asm: PathBuf,

    #[arg(
        long,
        short = 'b',
        value_name = "BIN_FILE",
        help = "Binary file output\n(contains the audio-driver, loader, blank-song, common-audio-data and songs)"
    )]
    output_bin: PathBuf,

    #[arg(
        long,
        short = 'i',
        value_name = "INC_FILE",
        help = "Output an include file containing song and sound effect enums (optional)"
    )]
    output_inc: Option<PathBuf>,

    #[arg(value_name = "PROJECT_FILE", help = "project file")]
    project_file: PathBuf,
}

fn export_with_asm_command<E: Exporter>(memory_map: &E::MemoryMap, args: ExportWithAsmArgs) {
    let relative_bin_path = match bin_include_path(&args.output_asm, &args.output_bin) {
        Ok(p) => p,
        Err(e) => error!("Error:  {}", e),
    };

    let (pf, bin_file) = load_and_export_project(&args);

    let asm_file = match E::generate_asm_file(&bin_file, memory_map, &relative_bin_path) {
        Ok(o) => o,
        Err(e) => error!("Error creating assembly file: {}", e),
    };

    let inc_file = match &args.output_inc {
        Some(path) => {
            let inc_file = match E::generate_include_file(pf) {
                Ok(o) => o,
                Err(e) => error!("Error creating enum include file: {}", e),
            };
            Some((path, inc_file))
        }
        None => None,
    };

    write_to_file(&args.output_bin, bin_file.data());
    write_to_file(&args.output_asm, asm_file.as_bytes());

    if let Some((inc_path, inc_str)) = inc_file {
        write_to_file(inc_path, inc_str.as_bytes());
    }
}

fn load_and_export_project(args: &ExportWithAsmArgs) -> (UniqueNamesProjectFile, ExportedBinFile) {
    let pf = load_project_file(&args.project_file);
    let (common_audio_data, songs) = compile_project(&pf);

    let bin_file = match export_bin_file(&common_audio_data, &songs) {
        Ok(b) => b,
        Err(e) => error!("Error: {}", e),
    };

    (pf, bin_file)
}

fn compile_and_check_song(
    song: &Song,
    pf: &UniqueNamesProjectFile,
    pitch_table: &PitchTable,
    common_data: &CommonAudioData,
) -> Result<SongData, String> {
    let mml_file = match load_text_file_with_limit(&song.source, &pf.parent_path) {
        Ok(tf) => tf,
        Err(e) => return Err(format!("Error compiling {}: {}", song.name, e)),
    };

    let song_data = match compile_mml(
        &mml_file,
        Some(song.name.clone()),
        &pf.instruments_and_samples,
        pitch_table,
    ) {
        Ok(mml) => mml,
        Err(e) => return Err(e.multiline_display().to_string()),
    };

    match validate_song_size(&song_data, common_data.data().len()) {
        Ok(()) => Ok(song_data),
        Err(e) => Err(format!(
            "Error compiling {}: {}",
            song.name,
            e.multiline_display()
        )),
    }
}

fn compile_project(pf: &UniqueNamesProjectFile) -> (CommonAudioData, Vec<SongData>) {
    let samples = build_sample_and_instrument_data(pf);
    if let Err(e) = samples {
        error!("{}", e.multiline_display())
    };

    let sfx = if let Ok(s) = &samples {
        compile_sound_effects(pf, s.pitch_table())
    } else {
        Err(())
    };

    let (samples, sfx) = match (samples, sfx) {
        (Ok(samples), Ok(sfx)) => (samples, sfx),
        _ => error!("Error compiling common audio data"),
    };

    let common_audio_data = match build_common_audio_data(&samples, &sfx) {
        Ok(data) => data,
        Err(e) => error!("{}", e.multiline_display()),
    };

    let mut compiled_songs = Vec::with_capacity(pf.songs.len());
    let mut n_song_errors = 0;

    for song in pf.songs.list().iter() {
        match compile_and_check_song(song, pf, samples.pitch_table(), &common_audio_data) {
            Ok(sd) => compiled_songs.push(sd),
            Err(e) => {
                n_song_errors += 1;
                eprintln!("{}\n", e);
            }
        }
    }

    if n_song_errors > 0 {
        if n_song_errors == 1 {
            error!("{} song has an error", n_song_errors);
        } else {
            error!("{} songs have errors", n_song_errors);
        }
    }

    assert!(compiled_songs.len() == pf.songs.len());

    (common_audio_data, compiled_songs)
}

//
// ca65-export
// ===========

#[derive(Args)]
struct Ca65ExportArgs {
    #[command(flatten)]
    base: ExportWithAsmArgs,

    #[command(flatten)]
    memory_map: MemoryMapModeArgument,

    #[arg(
        long = "segment",
        short = 's',
        value_name = "SEGMENT_NAME",
        requires = "mm_mode",
        help = "First segment to store the binary data in.\nMust be suffixed with a number (eg, RODATA2, AUDIO_DATA_0)\nIf data does not fit in a single bank, the next segment will be used (ie, RODATA3, AUDIO_DATA_1)"
    )]
    first_segment: String,
}

fn parse_ca65_memory_map(args: &Ca65ExportArgs) -> Ca65MemoryMap {
    match Ca65MemoryMap::try_new(args.memory_map.mode(), &args.first_segment) {
        Ok(mm) => mm,
        Err(e) => error!("Invalid memory map: {}", e),
    }
}

//
// pv-export
// =========

#[derive(Args)]
struct PvExportArgs {
    #[command(flatten)]
    base: ExportWithAsmArgs,

    #[command(flatten)]
    memory_map: MemoryMapModeArgument,

    #[arg(
        long = "bank",
        short = 'b',
        value_name = "BANK",
        requires = "mm_mode",
        default_value = "2",
        help = "The first wla-dx bank to store the binary data in.  Cannot be 1 or 0."
    )]
    first_bank: u8,
}

fn parse_pv_memory_map(args: &PvExportArgs) -> PvMemoryMap {
    match PvMemoryMap::try_new(args.memory_map.mode(), args.first_bank) {
        Ok(mm) => mm,
        Err(e) => error!("Invalid memory map: {}", e),
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
        Command::Check(args) => check_project_command(args),
        Command::Ca65Enums(args) => generate_enums_command::<Ca65Exporter>(args),
        Command::Ca65Export(args) => {
            export_with_asm_command::<Ca65Exporter>(&parse_ca65_memory_map(&args), args.base)
        }
        Command::PvEnums(args) => generate_enums_command::<PvExporter>(args),
        Command::PvExport(args) => {
            export_with_asm_command::<PvExporter>(&parse_pv_memory_map(&args), args.base)
        }
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

fn write_to_file(path: &Path, contents: &[u8]) {
    match std::fs::write(path, contents) {
        Ok(()) => (),
        Err(e) => error!("Error writing {}: {}", path.display(), e),
    }
}

mod output {
    use super::{write_to_file, OutputArg};

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
            write_to_file(path, data);
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
