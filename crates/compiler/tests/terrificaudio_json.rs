//! .terrificaudio project json reading and backwards compatibility tests

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use compiler::{
    data::{
        About, BlockNumber, BrrEvaluator, DefaultSfxFlags, Instrument, InstrumentNoteRange,
        LoopSetting, Name, Project, Sample, SampleNumber, Song,
    },
    envelope::{Adsr, Envelope, Gain},
    notes::Note,
    path::SourcePathBuf,
};

fn name(name: &str) -> Name {
    name.parse().unwrap()
}

fn source(path: &str) -> SourcePathBuf {
    SourcePathBuf::new_from_str(path)
}

fn octave_range(first: u32, last: u32) -> InstrumentNoteRange {
    InstrumentNoteRange::Octave {
        first: first.try_into().unwrap(),
        last: last.try_into().unwrap(),
    }
}

fn note_range(first: &str, last: &str) -> InstrumentNoteRange {
    InstrumentNoteRange::Note {
        first: Note::parse_bytecode_argument(first).unwrap(),
        last: Note::parse_bytecode_argument(last).unwrap(),
    }
}

// Testing TAD backwards compatibility with version 0.0.2
// (the oldest version with a example)
#[test]
fn version_0_0_2_example_project() {
    let json = r##"
{
  "_about": {
    "file_type": "Terrific Audio Driver project file",
    "version": "0.0.2"
  },
  "instruments": [

    {
      "name": "dbh",
      "source": "samples/dbh.wav",
      "freq": 500.0,
      "loop": "dupe_block_hack",
      "loop_setting": 3,
      "first_octave": 1,
      "last_octave": 6,
      "envelope": "adsr 12 1 1 16",
      "comment": null
    },
    {
      "name": "lrf",
      "source": "samples/lrf.wav",
      "freq": 500.0,
      "loop": "loop_reset_filter",
      "loop_setting": 0,
      "first_octave": 1,
      "last_octave": 6,
      "envelope": "adsr 12 1 1 16",
      "comment": null
    },
    {
      "name": "loop_with_filter",
      "source": "loop_with_filter.wav",
      "freq": 1000.0,
      "loop": "loop_with_filter",
      "loop_setting": 16,
      "first_octave": 3,
      "last_octave": 7,
      "envelope": "adsr 12 1 1 16",
      "comment": null
    },
    {
      "name": "wav_no_loop",
      "source": "wav_no_loop.wav",
      "freq": 500.0,
      "loop": "none",
      "first_octave": 1,
      "last_octave": 6,
      "envelope": "gain 254",
      "comment": "first example comment"
    },
    {
      "name": "first_brr_sample",
      "source": "samples/first_brr_sample.brr",
      "freq": 2000.0,
      "loop": "none",
      "first_octave": 1,
      "last_octave": 6,
      "envelope": "adsr 8 7 6 5",
      "comment": null
    },
    {
      "name": "second_brr_sample",
      "source": "../second_brr_sample.brr",
      "freq": 500.0,
      "loop": "override_brr_loop_point",
      "loop_setting": 32,
      "first_octave": 1,
      "last_octave": 5,
      "envelope": "gain 127",
      "comment": "second example comment"
    }
  ],
  "sound_effects": [
    "menu_cursor",
    "menu_select",
    "menu_incorrect"
  ],
  "sound_effect_file": "sound-effects.txt",
  "songs": [
    {
      "name": "gimo_297",
      "source": "songs/gimo-297.mml"
    },
    {
      "name": "ode_to_joy",
      "source": "songs/ode-to-joy.mml"
    }
  ]
}
"##;

    let expected = Project {
        about: About {
            version: "0.0.2".to_owned(),
        },
        instruments: vec![
            Instrument {
                name: name("dbh"),
                source: source("samples/dbh.wav"),
                freq: 500.0,
                loop_setting: LoopSetting::DupeBlockHack(BlockNumber(3)),
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                note_range: octave_range(1, 6),
                envelope: Envelope::Adsr(Adsr::try_new(12, 1, 1, 16).unwrap()),
                comment: None,
            },
            Instrument {
                name: name("lrf"),
                source: source("samples/lrf.wav"),
                freq: 500.0,
                loop_setting: LoopSetting::LoopResetFilter(SampleNumber(0)),
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                note_range: octave_range(1, 6),
                envelope: Envelope::Adsr(Adsr::try_new(12, 1, 1, 16).unwrap()),
                comment: None,
            },
            Instrument {
                name: name("loop_with_filter"),
                source: source("loop_with_filter.wav"),
                freq: 1000.0,
                loop_setting: LoopSetting::LoopWithFilter(SampleNumber(16)),
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                note_range: octave_range(3, 7),
                envelope: Envelope::Adsr(Adsr::try_new(12, 1, 1, 16).unwrap()),
                comment: None,
            },
            Instrument {
                name: name("wav_no_loop"),
                source: source("wav_no_loop.wav"),
                freq: 500.0,
                loop_setting: LoopSetting::None,
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                note_range: octave_range(1, 6),
                envelope: Envelope::Gain(Gain::new(254)),
                comment: Some("first example comment".to_owned()),
            },
            Instrument {
                name: name("first_brr_sample"),
                source: source("samples/first_brr_sample.brr"),
                freq: 2000.0,
                loop_setting: LoopSetting::None,
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                note_range: octave_range(1, 6),
                envelope: Envelope::Adsr(Adsr::try_new(8, 7, 6, 5).unwrap()),
                comment: None,
            },
            Instrument {
                name: name("second_brr_sample"),
                source: source("../second_brr_sample.brr"),
                freq: 500.0,
                loop_setting: LoopSetting::OverrideBrrLoopPoint(SampleNumber(32)),
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                note_range: octave_range(1, 5),
                envelope: Envelope::Gain("F127".parse().unwrap()),
                comment: Some("second example comment".to_owned()),
            },
        ],
        samples: vec![],
        default_sfx_flags: Default::default(),
        high_priority_sound_effects: vec![],
        sound_effects: vec![
            name("menu_cursor"),
            name("menu_select"),
            name("menu_incorrect"),
        ],
        low_priority_sound_effects: vec![],
        sound_effect_file: Some(source("sound-effects.txt")),
        songs: vec![
            Song {
                name: name("gimo_297"),
                source: source("songs/gimo-297.mml"),
            },
            Song {
                name: name("ode_to_joy"),
                source: source("songs/ode-to-joy.mml"),
            },
        ],
    };

    assert_eq!(serde_json::from_str::<Project>(json).unwrap(), expected);
}

// The version that added samples
#[test]
fn version_0_0_4() {
    let json = r##"
{
  "_about": {
    "file_type": "Terrific Audio Driver project file",
    "version": "0.0.4"
  },
  "instruments": [
    {
      "name": "dbh",
      "source": "samples/dbh.wav",
      "freq": 500.0,
      "loop": "dupe_block_hack",
      "loop_setting": 3,
      "first_octave": 1,
      "last_octave": 6,
      "envelope": "adsr 12 1 1 16",
      "comment": null
    },
    {
      "name": "lrf",
      "source": "samples/lrf.wav",
      "freq": 500.0,
      "loop": "loop_reset_filter",
      "loop_setting": 0,
      "first_octave": 1,
      "last_octave": 6,
      "envelope": "adsr 12 1 1 16",
      "comment": null
    },
    {
      "name": "loop_with_filter",
      "source": "loop_with_filter.wav",
      "freq": 1000.0,
      "loop": "loop_with_filter",
      "loop_setting": 16,
      "first_octave": 3,
      "last_octave": 7,
      "envelope": "adsr 12 1 1 16",
      "comment": null
    },
    {
      "name": "wav_no_loop",
      "source": "wav_no_loop.wav",
      "freq": 500.0,
      "loop": "none",
      "first_octave": 1,
      "last_octave": 6,
      "envelope": "gain B30",
      "comment": "first example comment"
    },
    {
      "name": "first_brr_sample",
      "source": "samples/first_brr_sample.brr",
      "freq": 2000.0,
      "loop": "none",
      "first_octave": 1,
      "last_octave": 6,
      "envelope": "adsr 8 7 6 5",
      "comment": null
    },
    {
      "name": "second_brr_sample",
      "source": "../second_brr_sample.brr",
      "freq": 500.0,
      "loop": "override_brr_loop_point",
      "loop_setting": 32,
      "first_octave": 1,
      "last_octave": 5,
      "envelope": "gain F127",
      "comment": "second example comment"
    }
  ],
  "samples": [
    {
      "name": "sample",
      "source": "sample.brr",
      "loop": "none",
      "sample_rates": [
        16000,
        18000
      ],
      "envelope": "adsr 5 6 7 8",
      "comment": null
    },
    {
      "name": "sample2",
      "source": "sample2.wav",
      "loop": "loop_filter_2",
      "loop_setting": 42,
      "sample_rates": [
        32000
      ],
      "envelope": "gain F127",
      "comment": "sample comment"
    }
  ],
  "sound_effects": [
    "menu_cursor",
    "menu_select",
    "menu_incorrect"
  ],
  "sound_effect_file": "sound-effects.txt",
  "songs": [
    {
      "name": "gimo_297",
      "source": "songs/gimo-297.mml"
    },
    {
      "name": "ode_to_joy",
      "source": "songs/ode-to-joy.mml"
    }
  ]
}
"##;

    let expected = Project {
        about: About {
            version: "0.0.4".to_owned(),
        },
        instruments: vec![
            Instrument {
                name: name("dbh"),
                source: source("samples/dbh.wav"),
                freq: 500.0,
                loop_setting: LoopSetting::DupeBlockHack(BlockNumber(3)),
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                note_range: octave_range(1, 6),
                envelope: Envelope::Adsr(Adsr::try_new(12, 1, 1, 16).unwrap()),
                comment: None,
            },
            Instrument {
                name: name("lrf"),
                source: source("samples/lrf.wav"),
                freq: 500.0,
                loop_setting: LoopSetting::LoopResetFilter(SampleNumber(0)),
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                note_range: octave_range(1, 6),
                envelope: Envelope::Adsr(Adsr::try_new(12, 1, 1, 16).unwrap()),
                comment: None,
            },
            Instrument {
                name: name("loop_with_filter"),
                source: source("loop_with_filter.wav"),
                freq: 1000.0,
                loop_setting: LoopSetting::LoopWithFilter(SampleNumber(16)),
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                note_range: octave_range(3, 7),
                envelope: Envelope::Adsr(Adsr::try_new(12, 1, 1, 16).unwrap()),
                comment: None,
            },
            Instrument {
                name: name("wav_no_loop"),
                source: source("wav_no_loop.wav"),
                freq: 500.0,
                loop_setting: LoopSetting::None,
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                note_range: octave_range(1, 6),
                envelope: Envelope::Gain(Gain::new(254)),
                comment: Some("first example comment".to_owned()),
            },
            Instrument {
                name: name("first_brr_sample"),
                source: source("samples/first_brr_sample.brr"),
                freq: 2000.0,
                loop_setting: LoopSetting::None,
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                note_range: octave_range(1, 6),
                envelope: Envelope::Adsr(Adsr::try_new(8, 7, 6, 5).unwrap()),
                comment: None,
            },
            Instrument {
                name: name("second_brr_sample"),
                source: source("../second_brr_sample.brr"),
                freq: 500.0,
                loop_setting: LoopSetting::OverrideBrrLoopPoint(SampleNumber(32)),
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                note_range: octave_range(1, 5),
                envelope: Envelope::Gain("F127".parse().unwrap()),
                comment: Some("second example comment".to_owned()),
            },
        ],
        samples: vec![
            Sample {
                name: name("sample"),
                source: source("sample.brr"),
                loop_setting: LoopSetting::None,
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                sample_rates: vec![16000, 18000],
                envelope: Envelope::Adsr(Adsr::try_new(5, 6, 7, 8).unwrap()),
                comment: None,
            },
            Sample {
                name: name("sample2"),
                source: source("sample2.wav"),
                loop_setting: LoopSetting::LoopFilter2(SampleNumber(42)),
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                sample_rates: vec![32000],
                envelope: Envelope::Gain("F127".parse().unwrap()),
                comment: Some("sample comment".to_owned()),
            },
        ],
        default_sfx_flags: Default::default(),
        high_priority_sound_effects: vec![],
        sound_effects: vec![
            name("menu_cursor"),
            name("menu_select"),
            name("menu_incorrect"),
        ],
        low_priority_sound_effects: vec![],
        sound_effect_file: Some(source("sound-effects.txt")),
        songs: vec![
            Song {
                name: name("gimo_297"),
                source: source("songs/gimo-297.mml"),
            },
            Song {
                name: name("ode_to_joy"),
                source: source("songs/ode-to-joy.mml"),
            },
        ],
    };

    assert_eq!(serde_json::from_str::<Project>(json).unwrap(), expected);
}

#[test]
fn version_0_0_10_sfx_priority() {
    let json = r##"
{
  "_about": {
    "file_type": "Terrific Audio Driver project file",
    "version": "0.0.10"
  },
  "instruments": [],
  "samples": [],
  "default_sfx_flags": {
    "one_channel": false,
    "interruptible": false
  },
  "high_priority_sound_effects": [
    "high_1",
    "high_2"
  ],
  "sound_effects": [
    "normal",
    "normal2",
    "normal3"
  ],
  "low_priority_sound_effects": [
    "low_priority_1",
    "low_priority_2"
  ],
  "sound_effect_file": null,
  "songs": []
}
"##;

    let expected = Project {
        about: About {
            version: "0.0.10".to_owned(),
        },
        instruments: vec![],
        samples: vec![],
        default_sfx_flags: DefaultSfxFlags {
            one_channel: false,
            interruptible: false,
        },
        high_priority_sound_effects: vec![name("high_1"), name("high_2")],
        sound_effects: vec![name("normal"), name("normal2"), name("normal3")],
        low_priority_sound_effects: vec![name("low_priority_1"), name("low_priority_2")],
        sound_effect_file: None,
        songs: vec![],
    };

    assert_eq!(serde_json::from_str::<Project>(json).unwrap(), expected);
}

#[test]
fn version_0_0_10_all_sfx_flags_set() {
    let json = r##"
{
  "_about": {
    "file_type": "Terrific Audio Driver project file",
    "version": "0.0.10"
  },
  "instruments": [],
  "samples": [],
  "default_sfx_flags": {
    "one_channel": true,
    "interruptible": true
  },
  "high_priority_sound_effects": [],
  "sound_effects": [],
  "low_priority_sound_effects": [],
  "sound_effect_file": null,
  "songs": []
}
"##;

    let expected = Project {
        about: About {
            version: "0.0.10".to_owned(),
        },
        instruments: vec![],
        samples: vec![],
        default_sfx_flags: DefaultSfxFlags {
            one_channel: true,
            interruptible: true,
        },
        high_priority_sound_effects: vec![],
        sound_effects: vec![],
        low_priority_sound_effects: vec![],
        sound_effect_file: None,
        songs: vec![],
    };

    assert_eq!(serde_json::from_str::<Project>(json).unwrap(), expected);
}

// Changes since 0.0.10 (I think)
//  * BrrEvaluator (0.0.14)
//  * Ignore_gaussian_overflow (0.0.14)
//  * Can set the first and last note in a instrument (0.2.0)
#[test]
fn version_0_3_0() {
    // This example should cover all fields and settings
    let json = r##"
{
  "_about": {
    "file_type": "Terrific Audio Driver project file",
    "version": "0.3.0"
  },
  "instruments": [
    {
      "name": "dbh",
      "source": "samples/dbh.wav",
      "freq": 500.0,
      "loop": "dupe_block_hack",
      "loop_setting": 3,
      "evaluator": "default",
      "ignore_gaussian_overflow": false,
      "first_octave": 1,
      "last_octave": 6,
      "envelope": "adsr 12 1 1 16",
      "comment": null
    },
    {
      "name": "lrf",
      "source": "samples/lrf.wav",
      "freq": 500.0,
      "loop": "loop_reset_filter",
      "loop_setting": 0,
      "evaluator": "default",
      "ignore_gaussian_overflow": true,
      "first_note": "c1",
      "last_note": "b6",
      "envelope": "adsr 12 1 1 16",
      "comment": null
    },
    {
      "name": "loop_with_filter",
      "source": "loop_with_filter.wav",
      "freq": 1000.0,
      "loop": "loop_with_filter",
      "loop_setting": 16,
      "evaluator": "default",
      "ignore_gaussian_overflow": false,
      "first_octave": 3,
      "last_octave": 7,
      "envelope": "adsr 12 1 1 16",
      "comment": null
    },
    {
      "name": "wav_no_loop",
      "source": "wav_no_loop.wav",
      "freq": 500.0,
      "loop": "none",
      "evaluator": "default",
      "ignore_gaussian_overflow": true,
      "first_note": "d+4",
      "last_note": "g5",
      "envelope": "gain B30",
      "comment": "first example comment"
    },
    {
      "name": "first_brr_sample",
      "source": "samples/first_brr_sample.brr",
      "freq": 2000.0,
      "loop": "none",
      "evaluator": "default",
      "ignore_gaussian_overflow": false,
      "first_octave": 1,
      "last_octave": 6,
      "envelope": "adsr 8 7 6 5",
      "comment": null
    },
    {
      "name": "second_brr_sample",
      "source": "../second_brr_sample.brr",
      "freq": 500.0,
      "loop": "override_brr_loop_point",
      "loop_setting": 32,
      "evaluator": "default",
      "ignore_gaussian_overflow": true,
      "first_octave": 1,
      "last_octave": 5,
      "envelope": "gain F127",
      "comment": "second example comment"
    },
    {
      "name": "dbh_1",
      "source": "samples/dbh.wav",
      "freq": 500.0,
      "loop": "dupe_block_hack_filter_1",
      "loop_setting": 2,
      "evaluator": "default",
      "ignore_gaussian_overflow": false,
      "first_octave": 1,
      "last_octave": 6,
      "envelope": "adsr 12 1 1 16",
      "comment": null
    },
    {
      "name": "dbh_2",
      "source": "samples/dbh.wav",
      "freq": 500.0,
      "loop": "dupe_block_hack_filter_2",
      "loop_setting": 3,
      "evaluator": "square_error",
      "ignore_gaussian_overflow": false,
      "first_octave": 1,
      "last_octave": 6,
      "envelope": "adsr 12 1 1 16",
      "comment": null
    },
    {
      "name": "dbh_3",
      "source": "samples/dbh.wav",
      "freq": 500.0,
      "loop": "dupe_block_hack_filter_3",
      "loop_setting": 4,
      "evaluator": "se_avoid_gaussian_overflow",
      "ignore_gaussian_overflow": true,
      "first_octave": 1,
      "last_octave": 6,
      "envelope": "adsr 12 1 1 16",
      "comment": null
    }
  ],
  "samples": [
    {
      "name": "sample",
      "source": "sample.brr",
      "loop": "none",
      "evaluator": "default",
      "ignore_gaussian_overflow": false,
      "sample_rates": [
        16000,
        18000
      ],
      "envelope": "adsr 5 6 7 8",
      "comment": null
    },
    {
      "name": "filter_1",
      "source": "filename.wav",
      "loop": "loop_filter_1",
      "loop_setting": 16,
      "evaluator": "se_avoid_gaussian_overflow",
      "ignore_gaussian_overflow": true,
      "sample_rates": [],
      "envelope": "gain F127",
      "comment": "sample comment"
    },
    {
      "name": "filter_2",
      "source": "filename.wav",
      "loop": "loop_filter_2",
      "loop_setting": 42,
      "evaluator": "square_error",
      "ignore_gaussian_overflow": false,
      "sample_rates": [8000],
      "envelope": "gain F127",
      "comment": null
    },
    {
      "name": "filter_3",
      "source": "filename.wav",
      "loop": "loop_filter_3",
      "loop_setting": 16,
      "evaluator": "default",
      "ignore_gaussian_overflow": true,
      "sample_rates": [6000],
      "envelope": "gain F127",
      "comment": null
    },
    {
      "name": "auto_filter",
      "source": "filename.wav",
      "loop": "loop_with_filter",
      "loop_setting": 20,
      "evaluator": "default",
      "ignore_gaussian_overflow": true,
      "sample_rates": [12000, 13000],
      "envelope": "gain F127",
      "comment": null
    }
  ],
  "default_sfx_flags": {
    "one_channel": false,
    "interruptible": true
  },
  "high_priority_sound_effects": [
    "highPriority"
  ],
  "sound_effects": [
    "normalPriority"
  ],
  "low_priority_sound_effects": [
    "lowPriority"
  ],
  "sound_effect_file": "sound-effects.txt",
  "songs": [
    {
      "name": "gimo_297",
      "source": "songs/gimo-297.mml"
    },
    {
      "name": "ode_to_joy",
      "source": "songs/ode-to-joy.mml"
    }
  ]
}
"##;

    let expected = Project {
        about: About {
            version: "0.3.0".to_owned(),
        },
        instruments: vec![
            Instrument {
                name: name("dbh"),
                source: source("samples/dbh.wav"),
                freq: 500.0,
                loop_setting: LoopSetting::DupeBlockHack(BlockNumber(3)),
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                note_range: octave_range(1, 6),
                envelope: Envelope::Adsr(Adsr::try_new(12, 1, 1, 16).unwrap()),
                comment: None,
            },
            Instrument {
                name: name("lrf"),
                source: source("samples/lrf.wav"),
                freq: 500.0,
                loop_setting: LoopSetting::LoopResetFilter(SampleNumber(0)),
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: true,
                note_range: note_range("c1", "b6"),
                envelope: Envelope::Adsr(Adsr::try_new(12, 1, 1, 16).unwrap()),
                comment: None,
            },
            Instrument {
                name: name("loop_with_filter"),
                source: source("loop_with_filter.wav"),
                freq: 1000.0,
                loop_setting: LoopSetting::LoopWithFilter(SampleNumber(16)),
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                note_range: octave_range(3, 7),
                envelope: Envelope::Adsr(Adsr::try_new(12, 1, 1, 16).unwrap()),
                comment: None,
            },
            Instrument {
                name: name("wav_no_loop"),
                source: source("wav_no_loop.wav"),
                freq: 500.0,
                loop_setting: LoopSetting::None,
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: true,
                note_range: note_range("d+4", "g5"),
                envelope: Envelope::Gain("B30".parse().unwrap()),
                comment: Some("first example comment".to_owned()),
            },
            Instrument {
                name: name("first_brr_sample"),
                source: source("samples/first_brr_sample.brr"),
                freq: 2000.0,
                loop_setting: LoopSetting::None,
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                note_range: octave_range(1, 6),
                envelope: Envelope::Adsr(Adsr::try_new(8, 7, 6, 5).unwrap()),
                comment: None,
            },
            Instrument {
                name: name("second_brr_sample"),
                source: source("../second_brr_sample.brr"),
                freq: 500.0,
                loop_setting: LoopSetting::OverrideBrrLoopPoint(SampleNumber(32)),
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: true,
                note_range: octave_range(1, 5),
                envelope: Envelope::Gain("F127".parse().unwrap()),
                comment: Some("second example comment".to_owned()),
            },
            Instrument {
                name: name("dbh_1"),
                source: source("samples/dbh.wav"),
                freq: 500.0,
                loop_setting: LoopSetting::DupeBlockHackFilter1(BlockNumber(2)),
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                note_range: octave_range(1, 6),
                envelope: Envelope::Adsr(Adsr::try_new(12, 1, 1, 16).unwrap()),
                comment: None,
            },
            Instrument {
                name: name("dbh_2"),
                source: source("samples/dbh.wav"),
                freq: 500.0,
                loop_setting: LoopSetting::DupeBlockHackFilter2(BlockNumber(3)),
                evaluator: BrrEvaluator::SquaredError,
                ignore_gaussian_overflow: false,
                note_range: octave_range(1, 6),
                envelope: Envelope::Adsr(Adsr::try_new(12, 1, 1, 16).unwrap()),
                comment: None,
            },
            Instrument {
                name: name("dbh_3"),
                source: source("samples/dbh.wav"),
                freq: 500.0,
                loop_setting: LoopSetting::DupeBlockHackFilter3(BlockNumber(4)),
                evaluator: BrrEvaluator::SquaredErrorAvoidGaussianOverflow,
                ignore_gaussian_overflow: true,
                note_range: octave_range(1, 6),
                envelope: Envelope::Adsr(Adsr::try_new(12, 1, 1, 16).unwrap()),
                comment: None,
            },
        ],
        samples: vec![
            Sample {
                name: name("sample"),
                source: source("sample.brr"),
                loop_setting: LoopSetting::None,
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: false,
                sample_rates: vec![16000, 18000],
                envelope: Envelope::Adsr(Adsr::try_new(5, 6, 7, 8).unwrap()),
                comment: None,
            },
            Sample {
                name: name("filter_1"),
                source: source("filename.wav"),
                loop_setting: LoopSetting::LoopFilter1(SampleNumber(16)),
                evaluator: BrrEvaluator::SquaredErrorAvoidGaussianOverflow,
                ignore_gaussian_overflow: true,
                sample_rates: vec![],
                envelope: Envelope::Gain(Gain::new(127)),
                comment: Some("sample comment".to_owned()),
            },
            Sample {
                name: name("filter_2"),
                source: source("filename.wav"),
                loop_setting: LoopSetting::LoopFilter2(SampleNumber(42)),
                evaluator: BrrEvaluator::SquaredError,
                ignore_gaussian_overflow: false,
                sample_rates: vec![8000],
                envelope: Envelope::Gain(Gain::new(127)),
                comment: None,
            },
            Sample {
                name: name("filter_3"),
                source: source("filename.wav"),
                loop_setting: LoopSetting::LoopFilter3(SampleNumber(16)),
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: true,
                sample_rates: vec![6000],
                envelope: Envelope::Gain(Gain::new(127)),
                comment: None,
            },
            Sample {
                name: name("auto_filter"),
                source: source("filename.wav"),
                loop_setting: LoopSetting::LoopWithFilter(SampleNumber(20)),
                evaluator: BrrEvaluator::Default,
                ignore_gaussian_overflow: true,
                sample_rates: vec![12000, 13000],
                envelope: Envelope::Gain(Gain::new(127)),
                comment: None,
            },
        ],
        default_sfx_flags: DefaultSfxFlags {
            one_channel: false,
            interruptible: true,
        },
        high_priority_sound_effects: vec![name("highPriority")],
        sound_effects: vec![name("normalPriority")],
        low_priority_sound_effects: vec![name("lowPriority")],
        sound_effect_file: Some(source("sound-effects.txt")),
        songs: vec![
            Song {
                name: name("gimo_297"),
                source: source("songs/gimo-297.mml"),
            },
            Song {
                name: name("ode_to_joy"),
                source: source("songs/ode-to-joy.mml"),
            },
        ],
    };

    assert_eq!(serde_json::from_str::<Project>(json).unwrap(), expected);
}
