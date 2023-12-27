//! wav2brr binary

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use brr::{encode_brr, read_16_bit_mono_wave_file, BrrFilter};

use clap::Parser;

use std::fs;
use std::path::PathBuf;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(after_help = "This command outputs brr files with a two-byte loop point header")]
#[command(arg_required_else_help = true)]
struct Args {
    #[arg(value_name = "WAV_FILE", help = "input wave file")]
    input: PathBuf,

    #[arg(short = 'o', long, value_name = "BRR_FILE", help = "output brr file")]
    output: PathBuf,

    #[arg(
        short = 'l',
        long,
        value_name = "N",
        help = "loop point (sample number)",
        conflicts_with = "dupe_block_hack"
    )]
    loop_point: Option<usize>,

    #[arg(
        long,
        value_name = "N",
        help = "Dupe-block-hack (number of blocks to duplicate)",
        conflicts_with = "loop_point"
    )]
    dupe_block_hack: Option<usize>,

    #[arg(
        short = 'r',
        long,
        help = "Reset BRR filter at the loop point",
        conflicts_with = "dupe_block_hack",
        conflicts_with = "loop_filter"
    )]
    loop_resets_filter: bool,

    #[arg(
        long,
        value_name = "FILTER",
        help = "Override BRR filter at the loop point",
        conflicts_with = "loop_resets_filter"
    )]
    loop_filter: Option<BrrFilter>,
}

macro_rules! error {
    ($($arg:tt)*) => {{
        eprintln!($($arg)*);
        std::process::exit(1);
    }};
}

fn main() {
    let args = Args::parse();

    let loop_filter = match (args.loop_resets_filter, args.loop_filter) {
        (false, f) => f,
        (true, None) => Some(BrrFilter::Filter0),
        (true, Some(_)) => {
            error!("Cannot use --loop-resets-filter and --loop-filter at the same time")
        }
    };

    let wav = {
        let mut wave_file = match fs::File::open(&args.input) {
            Err(why) => error!("Couldn't open {}: {}", args.input.display(), why),
            Ok(file) => file,
        };

        match read_16_bit_mono_wave_file(&mut wave_file, u16::MAX.into()) {
            Err(why) => error!("Error reading {}: {}", args.input.display(), why),
            Ok(wav) => wav,
        }
    };

    let brr = match encode_brr(
        &wav.samples,
        args.loop_point,
        args.dupe_block_hack,
        loop_filter,
    ) {
        Err(why) => error!("Cannot encode BRR: {}", why),
        Ok(brr) => brr,
    };

    let brr = brr.brr_with_loop_header();

    match fs::write(&args.output, brr) {
        Ok(()) => (),
        Err(why) => error!("Error writing {}: {}", args.output.display(), why),
    }
}
