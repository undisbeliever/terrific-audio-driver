#![no_main]

use compiler::pitch_table::{build_pitch_table, PitchTable};
use libfuzzer_sys::fuzz_target;

use compiler::data::{
    validate_instrument_and_sample_names, Instrument, InstrumentOrSample, LoopSetting, Name,
    Sample, UniqueNamesList,
};
use compiler::envelope::{Adsr, Envelope, Gain};
use compiler::notes::Octave;
use compiler::sound_effects::{compile_sound_effect, CompiledSfxSubroutines, SoundEffectText};

use std::str::FromStr;
use std::sync::OnceLock;

fn dummy_samples() -> &'static (UniqueNamesList<InstrumentOrSample>, PitchTable) {
    static LOCK: OnceLock<(UniqueNamesList<InstrumentOrSample>, PitchTable)> = OnceLock::new();

    // Names from `example-project.terrificaudio`
    LOCK.get_or_init(|| {
        let dummy_instruments = [
            Instrument {
                name: Name::from_str("sine").unwrap(),
                source: Default::default(),
                freq: 500.0,
                loop_setting: LoopSetting::None,
                evaluator: Default::default(),
                ignore_gaussian_overflow: false,
                first_octave: Octave::MIN,
                last_octave: Octave::try_new(5).unwrap(),
                envelope: Envelope::Adsr(Adsr::try_new(7, 2, 3, 28).unwrap()),
                comment: None,
            },
            Instrument {
                name: Name::from_str("square").unwrap(),
                source: Default::default(),
                freq: 500.0,
                loop_setting: LoopSetting::None,
                evaluator: Default::default(),
                ignore_gaussian_overflow: false,
                first_octave: Octave::try_new(2).unwrap(),
                last_octave: Octave::try_new(6).unwrap(),
                envelope: Envelope::Gain(Gain::new(127)),
                comment: None,
            },
            Instrument {
                name: Name::from_str("sawtooth").unwrap(),
                source: Default::default(),
                freq: 2000.0,
                loop_setting: LoopSetting::None,
                evaluator: Default::default(),
                ignore_gaussian_overflow: false,
                first_octave: Octave::try_new(3).unwrap(),
                last_octave: Octave::MAX,
                // Linear increase gain
                envelope: Envelope::Gain(Gain::new(0b110_10000)),
                comment: None,
            },
            Instrument {
                name: Name::from_str("triangle").unwrap(),
                source: Default::default(),
                freq: 1354.0,
                loop_setting: LoopSetting::None,
                evaluator: Default::default(),
                ignore_gaussian_overflow: false,
                first_octave: Octave::try_new(2).unwrap(),
                last_octave: Octave::try_new(6).unwrap(),
                // Bent-increase gain
                envelope: Envelope::Gain(Gain::new(0b111_11000)),
                comment: None,
            },
        ];
        let dummy_samples = [Sample {
            name: Name::from_str("sample").unwrap(),
            source: Default::default(),
            loop_setting: LoopSetting::None,
            evaluator: Default::default(),
            ignore_gaussian_overflow: false,
            sample_rates: vec![0, 16000, 18000, 32000],
            envelope: Envelope::Gain(Gain::new(127)),
            comment: None,
        }];

        let instruments_and_samples =
            validate_instrument_and_sample_names(dummy_instruments.iter(), dummy_samples.iter())
                .unwrap();

        let pitch_table = build_pitch_table(&instruments_and_samples).unwrap();

        (instruments_and_samples, pitch_table)
    })
}

fuzz_target!(|s: String| {
    let (instruments_and_samples, pitch_table) = dummy_samples();

    let _ = compile_sound_effect(
        &SoundEffectText::BytecodeAssembly(s),
        &instruments_and_samples,
        pitch_table,
        &CompiledSfxSubroutines::blank(),
        Default::default(),
    );
});
