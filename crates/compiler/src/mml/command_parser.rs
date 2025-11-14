//! MML command parser

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::note_tracking::CursorTracker;
use super::tokenizer::{MmlTokens, PeekableTokenIterator, Token};
use crate::bytecode_assembler;
use crate::identifier::{ChannelId, IdentifierStr};

use crate::bytecode::{
    DetuneValue, EarlyReleaseMinTicks, EarlyReleaseTicks, LoopCount, NoiseFrequency, Pan,
    PanSlideTicks, PanbrelloQuarterWavelengthInTicks, PlayNoteTicks, PlayPitchPitch,
    RelativeEchoFeedback, RelativeEchoVolume, RelativeFirCoefficient, Transpose, TremoloAmplitude,
    TremoloQuarterWavelengthInTicks, VibratoDelayTicks, VibratoPitchOffsetPerTick,
    VibratoQuarterWavelengthInTicks, Volume, VolumeSlideAmount, VolumeSlideTicks,
    KEY_OFF_TICK_DELAY,
};
use crate::command_compiler::commands::{
    merge_pan_commands, merge_volumes_commands, relative_pan, relative_volume, ChannelCommands,
    Command, CommandWithPos, DetuneCents, FineQuantization, ManualVibrato, MmlInstrument,
    MpVibrato, NoteOrPitch, PanCommand, Quantize, RestTicksAfterNote, SubroutineCallType,
    VolumeCommand,
};
use crate::driver_constants::FIR_FILTER_SIZE;
use crate::echo::{EchoVolume, FirCoefficient, FirTap};
use crate::envelope::{Gain, GainMode, OptionalGain, TempGain};
use crate::errors::{ChannelError, ErrorWithPos, ValueError};
use crate::file_pos::{FilePos, FilePosRange};
use crate::mml::GlobalSettings;
use crate::notes::{KeySignature, MidiNote, MmlPitch, Note, Octave, STARTING_OCTAVE};
use crate::pitch_table::PlayPitchSampleRate;
use crate::subroutines::SubroutineNameMap;
use crate::time::{MmlDefaultLength, MmlLength, TickCounter, ZenLen, STARTING_MML_LENGTH};
use crate::value_newtypes::{I8WithByteHexValueNewType, SignedValueNewType, UnsignedValueNewType};

pub use crate::command_compiler::commands::Quantization;

use std::collections::HashMap;

// The maximum default-length is `C255 l1........` = 502 ticks
pub const MAX_N_DOTS: u8 = 8;

pub const MAX_COARSE_VOLUME: u32 = 16;

pub const MIN_COARSE_TREMOLO_AMPLITUDE: u32 = 1;
pub const MAX_COARSE_TREMOLO_AMPLITUDE: u32 = 8;

pub const COARSE_VOLUME_MULTIPLIER: u8 = 16;

pub const PX_PAN_RANGE: std::ops::RangeInclusive<i32> =
    (-(Pan::CENTER.as_u8() as i32))..=(Pan::CENTER.as_u8() as i32);

// Parser state that is saved in CursorTracker accessed by the GUI
#[derive(Debug, Clone, PartialEq)]
pub struct State {
    pub zenlen: ZenLen,
    pub default_length: MmlDefaultLength,
    pub keyoff_enabled: bool,
    pub octave: Octave,
    pub semitone_offset: i8,
    pub signature: KeySignature,
    // Used by tad-gui statusbar
    pub quantize: Option<FineQuantization>,
}

mod parser {
    use crate::{
        command_compiler::commands::MmlInstrument, file_pos::LineIndexRange,
        mml::metadata::GlobalSettings,
    };

    use super::*;

    pub(super) struct Parser<'a, 'b> {
        channel: ChannelId,

        tokens: PeekableTokenIterator<'a>,
        errors: Vec<ErrorWithPos<ChannelError>>,
        state: State,

        default_length: TickCounter,
        keyoff_enabled: bool,

        instruments_map: &'b HashMap<IdentifierStr<'b>, &'b MmlInstrument>,
        subroutines: &'b dyn SubroutineNameMap,

        old_transpose: bool,

        tick_offset: TickCounter,
        cursor_tracker: &'b mut CursorTracker,
    }

    impl<'a, 'b> Parser<'a, 'b> {
        pub(super) fn new(
            channel: ChannelId,
            tokens: MmlTokens<'a>,
            instruments_map: &'b HashMap<IdentifierStr<'b>, &'b MmlInstrument>,
            subroutines: &'b dyn SubroutineNameMap,
            settings: &'b GlobalSettings,
            cursor_tracking: &'b mut CursorTracker,
        ) -> Parser<'a, 'b> {
            debug_assert!(matches!(tokens.first_token(), Some(Token::NewLine(_))));

            Parser {
                channel,
                tokens: PeekableTokenIterator::new(tokens),
                errors: Vec::new(),
                state: State {
                    zenlen: settings.zenlen,
                    default_length: STARTING_MML_LENGTH,
                    keyoff_enabled: true,
                    octave: STARTING_OCTAVE,
                    signature: settings.signature.clone(),
                    semitone_offset: settings.channel_transpose.as_i8(),
                    quantize: None,
                },

                default_length: settings.zenlen.starting_length(),
                keyoff_enabled: true,

                instruments_map,
                subroutines,

                old_transpose: settings.old_transpose,

                tick_offset: TickCounter::default(),
                cursor_tracker: cursor_tracking,
            }
        }

        pub(super) fn state(&self) -> &State {
            &self.state
        }

        pub(super) fn set_state(&mut self, new_state: State) {
            self.state = new_state;
            self.add_to_cursor_tracker();
        }

        pub(super) fn channel_id(&self) -> &ChannelId {
            &self.channel
        }

        // Must ONLY be called by the parsing macros in this module.
        pub(super) fn __peek(&self) -> &Token<'a> {
            self.tokens.peek()
        }

        // Must ONLY be called by the parsing macros in this module.
        pub(super) fn __next_token(&mut self) {
            self.tokens.next();
        }

        pub(super) fn peek_pos(&self) -> FilePos {
            self.tokens.peek_pos()
        }

        pub(super) fn file_pos_range_from(&self, pos: FilePos) -> FilePosRange {
            pos.to_range_pos(self.tokens.prev_end_pos())
        }

        pub(super) fn peek_and_next(&mut self) -> (FilePos, Token<'a>) {
            self.tokens.peek_and_next()
        }

        pub(super) fn add_error(&mut self, pos: FilePos, e: ChannelError) {
            self.errors
                .push(ErrorWithPos(self.file_pos_range_from(pos), e))
        }

        pub(super) fn instruments_map(&self) -> &HashMap<IdentifierStr<'b>, &MmlInstrument> {
            self.instruments_map
        }

        pub(super) fn find_subroutine(&self, name: &str) -> Option<u8> {
            self.subroutines.find_subroutine_index(name)
        }

        pub(super) fn old_transpose(&self) -> bool {
            self.old_transpose
        }

        pub(super) fn reset_tick_offset(&mut self) {
            self.tick_offset = TickCounter::new(0);
        }

        pub(super) fn increment_tick_offset(&mut self, t: TickCounter) {
            self.tick_offset += t;
            self.add_to_cursor_tracker();
        }

        pub(super) fn command_end_char_index(&self) -> u32 {
            if self.tick_offset.is_zero() {
                self.tokens.prev_end_pos().char_index
            } else {
                self.tokens.peek_pos().char_index
            }
        }

        pub(super) fn set_default_length(&mut self, ticks: TickCounter) {
            debug_assert!(ticks.value() <= 502);
            debug_assert!(ticks.value() < (1u32 << (MAX_N_DOTS + 1)));

            self.default_length = ticks;
        }

        pub(super) fn default_length(&self) -> TickCounter {
            self.default_length
        }

        pub(super) fn is_keyoff_enabled(&self) -> bool {
            self.keyoff_enabled
        }

        pub(super) fn set_keyoff_enabled(&mut self, keyoff: bool) {
            self.keyoff_enabled = keyoff;
        }

        pub(super) fn process_new_line(&mut self, r: LineIndexRange) {
            self.cursor_tracker
                .new_line(self.channel, r, self.tick_offset, self.state.clone());
        }

        fn add_to_cursor_tracker(&mut self) {
            self.cursor_tracker.add(
                self.tokens.prev_end_pos(),
                self.tick_offset,
                self.state.clone(),
            );
        }

        pub(super) fn finalize(self) -> Vec<ErrorWithPos<ChannelError>> {
            self.cursor_tracker.end_channel();

            self.errors
        }
    }
}
use parser::Parser;

// PARSING MACROS
// ==============
//
// These are the only things allowed to call `Parser::__peek()` and `Parser::__next_token()`.

// Builds a match statement for the tokenizer.
//
// Will consume the token and advance the tokenizer if there is a match
macro_rules! match_next_token {
    // Using `#` to separate patterns from the fallthrough no-matches case.
    ($parser:ident, $($pattern:pat $(if $guard:expr)? => $expression:expr,)+ #_ => $no_matches:expr) => {
        match $parser.__peek() {
        $(
            $pattern $(if $guard)? => {
                $parser.__next_token();
                $expression
            }
        )+

            _ => { $no_matches }
        }
    };
}

// Will consume the token and advance the tokenizer if there is a match
macro_rules! next_token_matches {
    ($parser:ident, $pattern:pat) => {
        match $parser.__peek() {
            $pattern => {
                $parser.__next_token();
                true
            }
            _ => false,
        }
    };
}

// Will not call `Parser::add_error()`
fn next_token_number(p: &mut Parser) -> Option<u32> {
    match_next_token!(
        p,

        &Token::Number(n) | &Token::HexNumber(n) => Some(n),
        #_ => None
    )
}

fn parse_unsigned_newtype<T>(pos: FilePos, p: &mut Parser) -> Option<T>
where
    T: UnsignedValueNewType,
{
    match_next_token!(
        p,

        &Token::Number(n) | &Token::HexNumber(n) => {
            match n.try_into() {
                Ok(o) => Some(o),
                Err(e) => {
                    p.add_error(pos, e.into());
                    None
                }
            }
        },
        #_ => {
            p.add_error(pos, T::MISSING_ERROR.into());
            None
        }
    )
}

fn parse_i8wh_newtype<T: I8WithByteHexValueNewType>(pos: FilePos, p: &mut Parser) -> T {
    let value_pos = p.peek_pos();

    match_next_token!(p,
        &Token::Number(n) => {
            match n.try_into() {
                Ok(n) => n,
                Err(e) => {
                    p.add_error(value_pos, e.into());
                    T::ZERO
                }
            }
        },
        &Token::HexNumber(h) => {
            match T::try_from_hex_byte(h) {
                Ok(h) => h,
                Err(e) => {
                    p.add_error(value_pos, e.into());
                    T::ZERO
                }
            }
        },
        &Token::RelativeNumber(n) => {
            match n.try_into() {
                Ok(n) => n,
                Err(e) => {
                    p.add_error(value_pos, e.into());
                    T::ZERO
                }
            }
        },
        #_ => {
            p.add_error(pos, T::MISSING_ERROR.into());
            T::ZERO
        }
    )
}

fn parse_signed_newtype<T>(pos: FilePos, p: &mut Parser) -> Option<T>
where
    T: SignedValueNewType,
{
    match_next_token!(
        p,

        &Token::RelativeNumber(n) => {
            match n.try_into() {
                Ok(o) => Some(o),
                Err(e) => {
                    p.add_error(pos, e.into());
                    None
                }
            }
        },
        &Token::Number(_) | &Token::HexNumber(_) => {
            p.add_error(pos, T::MISSING_SIGN_ERROR.into());
            None
        },
        #_ => {
            p.add_error(pos, T::MISSING_ERROR.into());
            None
        }
    )
}

fn parse_signed_newtype_allow_zero<T>(pos: FilePos, p: &mut Parser) -> Option<T>
where
    T: SignedValueNewType,
{
    match_next_token!(
        p,

        &Token::RelativeNumber(n) => {
            match n.try_into() {
                Ok(o) => Some(o),
                Err(e) => {
                    p.add_error(pos, e.into());
                    None
                }
            }
        },
        &Token::Number(0) | &Token::HexNumber(0) => {
            match 0.try_into() {
                Ok(o) => Some(o),
                Err(e) => {
                    p.add_error(pos, e.into());
                    None
                }
            }
        },
        &Token::Number(_) | &Token::HexNumber(_) => {
            p.add_error(pos, T::MISSING_SIGN_ERROR.into());
            None
        },
        #_ => {
            p.add_error(pos, T::MISSING_ERROR.into());
            None
        }
    )
}

trait CommaTicks: UnsignedValueNewType {
    const NO_COMMA_ERROR: ValueError;
}

impl CommaTicks for VibratoQuarterWavelengthInTicks {
    const NO_COMMA_ERROR: ValueError = ValueError::NoCommaQuarterWavelength;
}

impl CommaTicks for VolumeSlideTicks {
    const NO_COMMA_ERROR: ValueError = ValueError::NoCommaVolumeSlideTicks;
}

impl CommaTicks for TremoloQuarterWavelengthInTicks {
    const NO_COMMA_ERROR: ValueError = ValueError::NoCommaQuarterWavelength;
}

impl CommaTicks for PanSlideTicks {
    const NO_COMMA_ERROR: ValueError = ValueError::NoCommaPanSlideTicks;
}

impl CommaTicks for PanbrelloQuarterWavelengthInTicks {
    const NO_COMMA_ERROR: ValueError = ValueError::NoCommaQuarterWavelength;
}

fn parse_comma_ticks<T: CommaTicks>(pos: FilePos, p: &mut Parser) -> Option<T> {
    if next_token_matches!(p, Token::Comma) {
        let after_pos = p.peek_pos();

        match_next_token!(
            p,

            Token::SetDefaultLength => parse_l_ticks_argument(pos, p),

            &Token::Number(n) | &Token::HexNumber(n) => {
                match n.try_into() {
                    Ok(o) => Some(o),
                    Err(e) => {
                        p.add_error(after_pos, e.into());
                        None
                    }
                }
            },
            #_ => {
                p.add_error(pos, T::MISSING_ERROR.into());
                None
            }
        )
    } else {
        p.add_error(pos, T::NO_COMMA_ERROR.into());
        None
    }
}

fn parse_optional_comma_ticks<T: UnsignedValueNewType>(p: &mut Parser) -> Option<T> {
    let comma_pos = p.peek_pos();

    if next_token_matches!(p, Token::Comma) {
        let after_pos = p.peek_pos();

        match_next_token!(
            p,

            Token::SetDefaultLength => parse_l_ticks_argument(comma_pos, p),

            &Token::Number(n) | &Token::HexNumber(n) => {
                match n.try_into() {
                    Ok(o) => Some(o),
                    Err(e) => {
                        p.add_error(after_pos, e.into());
                        None
                    }
                }
            },
            #_ => {
                p.add_error(comma_pos, T::MISSING_ERROR.into());
                None
            }
        )
    } else {
        None
    }
}

fn parse_l_ticks_argument<T: UnsignedValueNewType>(pos: FilePos, p: &mut Parser) -> Option<T> {
    match parse_untracked_optional_mml_length(p) {
        Some(l) => match l.to_tick_count(p.default_length(), p.state().zenlen) {
            Ok(tc) => match tc.value().try_into() {
                Ok(o) => Some(o),
                Err(e) => {
                    p.add_error(pos, e.into());
                    None
                }
            },
            Err(e) => {
                p.add_error(pos, e.into());
                None
            }
        },
        None => {
            p.add_error(pos, T::MISSING_ERROR.into());
            None
        }
    }
}

fn parse_play_pitch_sample_rate_value(pos: FilePos, p: &mut Parser) -> PlayPitchPitch {
    match parse_unsigned_newtype::<PlayPitchSampleRate>(pos, p) {
        Some(sr) => sr.to_vxpitch(),
        None => PlayPitchPitch::NATIVE,
    }
}

fn parse_dots_after_length(p: &mut Parser) -> u8 {
    let mut number_of_dots = 0;
    while next_token_matches!(p, Token::Dot) {
        if number_of_dots <= MAX_N_DOTS {
            number_of_dots += 1;
        }
    }

    number_of_dots
}

fn parse_set_default_length(pos: FilePos, p: &mut Parser) {
    let length_in_ticks = next_token_matches!(p, Token::PercentSign);
    let length = match_next_token!(
        p,
        &Token::Number(n) | &Token::HexNumber(n) => match n.try_into() {
            Ok(n) => n,
            Err(_) => {
                p.add_error(pos, ValueError::InvalidDefaultLength.into());
                return;
            }
        },
        #_ => {
            p.add_error(pos, ValueError::MissingDefaultLength.into());
            return;
        }
    );

    let number_of_dots = parse_dots_after_length(p);

    let default_length = MmlDefaultLength::new(length, length_in_ticks, number_of_dots);

    match default_length.to_tick_count(p.state().zenlen) {
        Ok(tc) => {
            p.set_default_length(tc);
            p.set_state(State {
                default_length,
                ..p.state().clone()
            });
        }
        Err(e) => {
            p.add_error(pos, e.into());
        }
    }
}

fn parse_change_whole_note_length(pos: FilePos, p: &mut Parser) {
    if let Some(new_zenlen) = parse_unsigned_newtype(pos, p) {
        let state = p.state().clone();

        let tc = match state.default_length.to_tick_count(new_zenlen) {
            Ok(tc) => tc,
            Err(e) => {
                p.add_error(pos, e.into());
                p.default_length()
            }
        };
        p.set_default_length(tc);
        p.set_state(State {
            zenlen: new_zenlen,
            ..p.state().clone()
        });
    }
}

fn parse_set_octave(pos: FilePos, p: &mut Parser) {
    if let Some(o) = parse_unsigned_newtype(pos, p) {
        p.set_state(State {
            octave: o,
            ..p.state().clone()
        });
    }
}

fn parse_increment_octave(p: &mut Parser) {
    let mut o = p.state().octave;
    o.saturating_increment();

    if o != p.state().octave {
        p.set_state(State {
            octave: o,
            ..p.state().clone()
        })
    }
}

fn parse_decrement_octave(p: &mut Parser) {
    let mut o = p.state().octave;
    o.saturating_decrement();

    if o != p.state().octave {
        p.set_state(State {
            octave: o,
            ..p.state().clone()
        })
    }
}

#[must_use]
fn parse_transpose<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    match p.old_transpose() {
        false => match parse_signed_newtype_allow_zero(pos, p) {
            Some(t) => Command::SetTranspose(t),
            None => Command::None,
        },
        true => {
            parse_channel_transpose(pos, p);
            Command::None
        }
    }
}

#[must_use]
fn parse_relative_transpose<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    match p.old_transpose() {
        false => match parse_signed_newtype(pos, p) {
            Some(r) => Command::AdjustTranspose(r),
            None => Command::None,
        },
        true => {
            parse_relative_channel_transpose(pos, p);
            Command::None
        }
    }
}

fn parse_channel_transpose(pos: FilePos, p: &mut Parser) {
    if let Some(t) = parse_signed_newtype_allow_zero(pos, p) {
        let t: Transpose = t;
        p.set_state(State {
            semitone_offset: t.as_i8(),
            ..p.state().clone()
        });
    }
}

fn parse_relative_channel_transpose(pos: FilePos, p: &mut Parser) {
    if let Some(rt) = parse_signed_newtype(pos, p) {
        let rt: Transpose = rt;

        let state = p.state().clone();
        p.set_state(State {
            semitone_offset: state.semitone_offset.saturating_add(rt.as_i8()),
            ..state
        });
    }
}

fn parse_key_signature(pos: FilePos, p: &mut Parser, s: &str) {
    match p.state().signature.parse_signature_changes(s) {
        Ok(signature) => {
            p.set_state(State {
                signature,
                ..p.state().clone()
            });
        }
        Err(e) => {
            p.add_error(pos, e.into());
        }
    }
}

fn parse_optional_gain_value(pos: FilePos, p: &mut Parser, mode: GainMode) -> OptionalGain {
    match next_token_number(p) {
        Some(v) => match OptionalGain::try_from_mode_and_value_forbid_none(mode, v) {
            Ok(g) => g,
            Err(e) => {
                p.add_error(pos, e.into());
                OptionalGain::NONE
            }
        },
        None => {
            p.add_error(pos, ValueError::NoOptionalGainValue.into());
            OptionalGain::NONE
        }
    }
}

fn parse_quantize_optional_comma_optional_gain(p: &mut Parser) -> OptionalGain {
    if next_token_matches!(p, Token::Comma) {
        let pos = p.peek_pos();

        let mode = match_next_token!(
            p,

            Token::GainModeF => GainMode::Fixed,
            Token::DetuneOrGainModeD => GainMode::LinearDecrease,
            Token::Echo => GainMode::ExponentialDecrease,
            Token::GainModeI => GainMode::LinearIncrease,
            Token::GainModeB => GainMode::BentIncrease,

            #_ => GainMode::Raw
        );

        parse_optional_gain_value(pos, p, mode)
    } else {
        OptionalGain::NONE
    }
}

fn parse_quantize<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    let (q, g) = match_next_token!(
        p,

        Token::PercentSign => {
            let q = parse_unsigned_newtype::<FineQuantization>(pos, p);
            let g = parse_quantize_optional_comma_optional_gain(p);

            (q, g)
        },

        #_ => {
            let q = parse_unsigned_newtype::<Quantization>(pos, p);
            let g = parse_quantize_optional_comma_optional_gain(p);

            (q.and_then(|q| q.to_fine()), g)
        }
    );

    p.set_state(State {
        quantize: q,
        ..p.state().clone()
    });

    match (q, g.is_none()) {
        (None, _) => Command::SetQuantize(Quantize::None),
        (Some(q), true) => Command::SetQuantize(Quantize::Rest(q)),
        (Some(q), false) => Command::SetQuantize(Quantize::WithTempGain(q, g.into())),
    }
}

fn parse_early_release_gain_argument(comma_pos: FilePos, p: &mut Parser) -> OptionalGain {
    let pos = p.peek_pos();
    match_next_token!(
        p,

        Token::GainModeF => parse_optional_gain_value(pos, p, GainMode::Fixed),
        Token::DetuneOrGainModeD => parse_optional_gain_value(pos, p, GainMode::LinearDecrease),
        Token::Echo => parse_optional_gain_value(pos, p, GainMode::ExponentialDecrease),
        Token::GainModeI => parse_optional_gain_value(pos, p, GainMode::LinearIncrease),
        Token::GainModeB => parse_optional_gain_value(pos, p, GainMode::BentIncrease),
        #_ => {
            // Ignore raw GAIN number
            let _ = next_token_matches!(p, Token::Number(_) | Token::HexNumber(_));
            p.add_error(comma_pos, ValueError::NoOptionalGainMode.into());
            OptionalGain::NONE
        }
    )
}

fn parse_set_early_release_arguments(p: &mut Parser) -> (EarlyReleaseMinTicks, OptionalGain) {
    let min = EarlyReleaseMinTicks::MIN;

    let comma_pos = p.peek_pos();
    if next_token_matches!(p, Token::Comma) {
        let pos = p.peek_pos();

        match_next_token!(
            p,

            Token::GainModeF => (min, parse_optional_gain_value(pos, p, GainMode::Fixed)),
            Token::DetuneOrGainModeD => (min, parse_optional_gain_value(pos, p, GainMode::LinearDecrease)),
            Token::Echo => (min, parse_optional_gain_value(pos, p, GainMode::ExponentialDecrease)),
            Token::GainModeI => (min, parse_optional_gain_value(pos, p, GainMode::LinearIncrease)),
            Token::GainModeB => (min, parse_optional_gain_value(pos, p, GainMode::BentIncrease)),

            // min field is blank
            &Token::Comma => (min, parse_early_release_gain_argument(pos, p)),

            &Token::Number(n) | &Token::HexNumber(n) => {
                let min = match n.try_into() {
                    Ok(o) => o,
                    Err(e) => {
                        p.add_error(pos, ChannelError::ValueError(e));
                        min
                    }
                };
                let comma_pos = p.peek_pos();
                match_next_token!(
                    p,
                    Token::Comma => (min, parse_early_release_gain_argument(comma_pos, p)),
                    #_ => (min, OptionalGain::NONE)
                )
            },

            Token::SetDefaultLength => {
                let min = parse_l_ticks_argument(pos, p).unwrap_or(min);

                let comma_pos = p.peek_pos();
                match_next_token!(
                    p,
                    Token::Comma => (min, parse_early_release_gain_argument(comma_pos, p)),
                    #_ => (min, OptionalGain::NONE)
                )
            },

            #_ => {
                p.add_error(comma_pos, ValueError::NoEarlyReleaseMinTicksOrGain.into());
                (min, OptionalGain::NONE)
            }
        )
    } else {
        (min, OptionalGain::NONE)
    }
}

fn parse_set_early_release<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    match_next_token!(
        p,

        &Token::Number(0) | &Token::HexNumber(0) => {
            Command::DisableEarlyRelease
        },

        &Token::Number(n) | &Token::HexNumber(n) => {
            let ticks = match n.try_into() {
                Ok(t) => t,
                Err(e) => {
                    p.add_error(pos, ChannelError::ValueError(e));
                    EarlyReleaseTicks::MIN
                }
            };
            let (min, g) = parse_set_early_release_arguments(p);

            Command::SetEarlyRelease(ticks, min, g)
        },

        Token::SetDefaultLength => {
            let ticks = parse_l_ticks_argument(pos, p).unwrap_or(EarlyReleaseTicks::MIN);
            let (min, g) = parse_set_early_release_arguments(p);
            Command::SetEarlyRelease(ticks, min, g)
        },
        #_ => {
            p.add_error(pos, ValueError::NoEarlyReleaseTicks.into());

            parse_set_early_release_arguments(p);
            Command::None
        }
    )
}

fn parse_detune<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    Command::SetDetune(parse_signed_newtype_allow_zero(pos, p).unwrap_or(DetuneValue::ZERO))
}

fn parse_detune_cents<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    Command::SetDetuneCents(parse_signed_newtype_allow_zero(pos, p).unwrap_or(DetuneCents::ZERO))
}

// Returns true if the token was recognised and processed
fn merge_state_change(p: &mut Parser) -> bool {
    let pos = p.peek_pos();

    match_next_token!(
        p,

        &Token::NewLine(r) => {
            p.process_new_line(r);
            true
        },

        Token::SetOctave => {
            parse_set_octave(pos, p);
            true
        },
        Token::IncrementOctave => {
            parse_increment_octave(p);
            true
        },
        Token::DecrementOctave => {
            parse_decrement_octave(p);
            true
        },
        Token::Transpose if p.old_transpose() => {
            parse_channel_transpose(pos, p);
            true
        },
        Token::RelativeTranspose if p.old_transpose() => {
            parse_relative_channel_transpose(pos, p);
            true
        },

        Token::ChannelTranspose => {
            parse_channel_transpose(pos, p);
            true
        },
        Token::RelativeChannelTranspose => {
            parse_relative_channel_transpose(pos, p);
            true
        },

        Token::SetDefaultLength => {
            parse_set_default_length(pos, p);
            true
        },
        Token::ChangeWholeNoteLength => {
            parse_change_whole_note_length(pos, p);
            true
        },
        Token::Divider => {
            true
        },
        #_ => false
    )
}

fn parse_tracked_length(p: &mut Parser) -> TickCounter {
    let pos = p.peek_pos();

    let length_in_ticks = next_token_matches!(p, Token::PercentSign);

    let length = match_next_token!(
        p,
        &Token::Number(n) | &Token::HexNumber(n) => Some(n),
        #_ => None
    );

    let number_of_dots = parse_dots_after_length(p);

    let note_length = MmlLength::new(length, length_in_ticks, number_of_dots);

    let length = match note_length.to_tick_count(p.default_length(), p.state().zenlen) {
        Ok(tc) => tc,
        Err(e) => {
            p.add_error(pos, e.into());
            p.default_length()
        }
    };

    p.increment_tick_offset(length);

    length
}

// Assumes comman-length is at the end of the command
fn parse_tracked_comma_length(p: &mut Parser) -> TickCounter {
    let pos = p.peek_pos();

    let (length_in_ticks, length) = match_next_token!(p,
        Token::Comma => {
            let length_in_ticks = next_token_matches!(p, Token::PercentSign);
            let length = match_next_token!(
                p,
                &Token::Number(n) | &Token::HexNumber(n) => Some(n),
                #_ => {
                    p.add_error(pos, ChannelError::NoLengthAfterComma);
                    None
                }
            );
            (length_in_ticks, length)
        },
        #_ => {
            (false, None)
        }
    );

    let number_of_dots = parse_dots_after_length(p);

    let note_length = MmlLength::new(length, length_in_ticks, number_of_dots);

    let length = match note_length.to_tick_count(p.default_length(), p.state().zenlen) {
        Ok(tc) => tc,
        Err(e) => {
            p.add_error(pos, e.into());
            p.default_length()
        }
    };

    p.increment_tick_offset(length);

    length
}

fn parse_tracked_optional_length(p: &mut Parser) -> Option<TickCounter> {
    let length = parse_untracked_optional_length(p);
    if let Some(tc) = length {
        p.increment_tick_offset(tc)
    }
    length
}

fn parse_untracked_optional_mml_length(p: &mut Parser) -> Option<MmlLength> {
    let length_in_ticks = next_token_matches!(p, Token::PercentSign);

    let length = match_next_token!(
        p,
        &Token::Number(n) | &Token::HexNumber(n) => Some(n),
        #_ => None
    );

    let number_of_dots = parse_dots_after_length(p);

    if length_in_ticks || length.is_some() || number_of_dots > 0 {
        Some(MmlLength::new(length, length_in_ticks, number_of_dots))
    } else {
        None
    }
}

fn parse_untracked_optional_length(p: &mut Parser) -> Option<TickCounter> {
    let pos = p.peek_pos();

    let note_length = parse_untracked_optional_mml_length(p)?;
    let tc = match note_length.to_tick_count(p.default_length(), p.state().zenlen) {
        Ok(tc) => tc,
        Err(e) => {
            p.add_error(pos, e.into());
            p.default_length()
        }
    };
    Some(tc)
}

fn parse_pitch_list_state_change_token(token: Token, pos: FilePos, p: &mut Parser) {
    match token {
        Token::SetOctave => parse_set_octave(pos, p),
        Token::IncrementOctave => parse_increment_octave(p),
        Token::DecrementOctave => parse_decrement_octave(p),
        Token::ChannelTranspose => parse_channel_transpose(pos, p),
        Token::RelativeChannelTranspose => parse_relative_channel_transpose(pos, p),
        Token::Transpose if p.old_transpose() => parse_channel_transpose(pos, p),
        Token::RelativeTranspose if p.old_transpose() => parse_relative_channel_transpose(pos, p),

        _ => p.add_error(pos, ChannelError::InvalidPitchListSymbol),
    }
}

fn parse_broken_chord_pitches(p: &mut Parser) -> Option<(Vec<NoteOrPitch>, FilePos)> {
    let mut out = Vec::new();

    loop {
        let (pos, token) = p.peek_and_next();

        match token {
            Token::EndBrokenChord => {
                return Some((out, pos));
            }

            Token::EndPortamento | Token::End => {
                p.add_error(pos, ChannelError::MissingEndBrokenChord);
                return None;
            }
            Token::NewLine(r) => {
                p.add_error(pos, ChannelError::MissingEndBrokenChord);
                p.process_new_line(r);
                return None;
            }

            Token::Pitch(pitch) => match pitch_to_note(pitch, p) {
                Ok(note) => out.push(NoteOrPitch::Note(note)),
                Err(e) => p.add_error(pos, e),
            },

            Token::PlayPitch => {
                if let Some(p) = parse_unsigned_newtype(pos, p) {
                    out.push(NoteOrPitch::Pitch(p));
                }
            }

            Token::PlayPitchSampleRate => {
                let p = parse_play_pitch_sample_rate_value(pos, p);
                out.push(NoteOrPitch::Pitch(p));
            }

            Token::PlayPitchFrequency => {
                if let Some(p) = parse_unsigned_newtype(pos, p) {
                    out.push(NoteOrPitch::PitchFrequency(p));
                }
            }

            _ => {
                parse_pitch_list_state_change_token(token, pos, p);
            }
        }
    }
}

enum PortamentoPitch {
    Ok(NoteOrPitch),
    End,
    MissingEnd,
}

fn parse_portamento_pitch(p: &mut Parser) -> PortamentoPitch {
    loop {
        let (pos, token) = p.peek_and_next();

        match token {
            Token::EndPortamento => {
                return PortamentoPitch::End;
            }

            Token::EndBrokenChord | Token::End => {
                p.add_error(pos, ChannelError::MissingEndPortamento);
                return PortamentoPitch::MissingEnd;
            }
            Token::NewLine(r) => {
                p.add_error(pos, ChannelError::MissingEndPortamento);
                p.process_new_line(r);
                return PortamentoPitch::MissingEnd;
            }

            Token::Pitch(pitch) => match pitch_to_note(pitch, p) {
                Ok(n) => return PortamentoPitch::Ok(NoteOrPitch::Note(n)),
                Err(e) => p.add_error(pos, e),
            },

            Token::PlayPitch => {
                if let Some(p) = parse_unsigned_newtype(pos, p) {
                    return PortamentoPitch::Ok(NoteOrPitch::Pitch(p));
                }
            }

            Token::PlayPitchSampleRate => {
                let p = parse_play_pitch_sample_rate_value(pos, p);
                return PortamentoPitch::Ok(NoteOrPitch::Pitch(p));
            }

            Token::PlayPitchFrequency => {
                if let Some(p) = parse_unsigned_newtype(pos, p) {
                    return PortamentoPitch::Ok(NoteOrPitch::PitchFrequency(p));
                }
            }

            _ => {
                parse_pitch_list_state_change_token(token, pos, p);
            }
        }
    }
}

enum PortamentoPitchError {
    WrongCount,
    MissingEnd,
}

fn parse_portamento_pitches(
    start_pos: FilePos,
    p: &mut Parser,
) -> Result<(Option<NoteOrPitch>, NoteOrPitch), PortamentoPitchError> {
    let note1 = match parse_portamento_pitch(p) {
        PortamentoPitch::Ok(n) => n,

        PortamentoPitch::End => {
            p.add_error(start_pos, ChannelError::PortamentoRequiresTwoPitches);
            return Err(PortamentoPitchError::WrongCount);
        }

        PortamentoPitch::MissingEnd => {
            return Err(PortamentoPitchError::MissingEnd);
        }
    };

    let note2 = match parse_portamento_pitch(p) {
        PortamentoPitch::Ok(n) => n,

        PortamentoPitch::End => return Ok((None, note1)),

        PortamentoPitch::MissingEnd => {
            return Err(PortamentoPitchError::MissingEnd);
        }
    };

    match parse_portamento_pitch(p) {
        PortamentoPitch::End => Ok((Some(note1), note2)),

        PortamentoPitch::MissingEnd => Err(PortamentoPitchError::MissingEnd),

        PortamentoPitch::Ok(_) => {
            // more than 2 notes
            loop {
                match parse_portamento_pitch(p) {
                    PortamentoPitch::End => {
                        p.add_error(start_pos, ChannelError::PortamentoRequiresTwoPitches);
                        return Err(PortamentoPitchError::WrongCount);
                    }
                    PortamentoPitch::MissingEnd => {
                        return Err(PortamentoPitchError::MissingEnd);
                    }
                    // Skip extra pitches
                    PortamentoPitch::Ok(_) => {}
                }
            }
        }
    }
}

fn parse_pan_value(pos: FilePos, p: &mut Parser) -> Option<PanCommand> {
    match_next_token!(
        p,

        &Token::Number(n) | &Token::HexNumber(n) => {
            match n.try_into() {
                Ok(v) => Some(PanCommand::Absolute(v)),
                Err(e) => {
                    p.add_error(pos, e.into());
                    None
                }
            }
        },
        &Token::RelativeNumber(n) => {
            Some(relative_pan(n))
        },
        #_ => {
            p.add_error(pos, ChannelError::NoPanParameter);
            None
        }
    )
}

fn parse_px_pan_value(pos: FilePos, p: &mut Parser) -> Option<PanCommand> {
    match_next_token!(
        p,

        &Token::Number(0) | &Token::HexNumber(0) => {
            Some(PanCommand::Absolute(Pan::CENTER))
        },
        &Token::RelativeNumber(n) => {
            if PX_PAN_RANGE.contains(&n) {
                let p = n + i32::from(Pan::CENTER.as_u8());
                Some(PanCommand::Absolute(Pan::try_from(u8::try_from(p).unwrap()).unwrap()))
            }
            else {
                p.add_error(pos, ValueError::PxPanOutOfRange(n).into());
                None
            }
        },
        &Token::Number(_) | &Token::HexNumber(_) => {
            p.add_error(pos, ValueError::NoPxPanSign.into());
            None
        },
        #_ => {
            p.add_error(pos, ValueError::NoPxPan.into());
            None
        }
    )
}

fn parse_fine_volume_value(pos: FilePos, p: &mut Parser) -> Option<VolumeCommand> {
    match_next_token!(
        p,

        &Token::Number(n) | &Token::HexNumber(n) => {
            match n.try_into() {
                Ok(v) => Some(VolumeCommand::Absolute(v)),
                Err(e) => {
                    p.add_error(pos, e.into());
                    None
                }
            }
        },
        &Token::RelativeNumber(n) => {
            Some(relative_volume(n))
        },
        #_ => {
            p.add_error(pos, ChannelError::NoFineVolumeParameter);
            None
        }
    )
}

fn parse_coarse_volume_value(pos: FilePos, p: &mut Parser) -> Option<VolumeCommand> {
    match_next_token!(
        p,

        &Token::Number(v) | &Token::HexNumber(v) => {
            if v <= MAX_COARSE_VOLUME {
                let v = u8::try_from(v).unwrap();
                let v = v.saturating_mul(COARSE_VOLUME_MULTIPLIER);
                Some(VolumeCommand::Absolute(Volume::new(v)))
            }
            else {
                p.add_error(pos, ValueError::CoarseVolumeOutOfRange(v).into());
                None
            }
        },
        &Token::RelativeNumber(n) => {
            let v = n.saturating_mul(COARSE_VOLUME_MULTIPLIER.into());
            Some(relative_volume(v))
        },
        #_ => {
            p.add_error(pos, ChannelError::NoCoarseVolumeParameter);
            None
        }
    )
}

fn parse_dec_volume_paren(pos: FilePos, p: &mut Parser) -> VolumeCommand {
    let rv = |v: u32| match i32::try_from(v) {
        Ok(v) => relative_volume(-v),
        Err(_) => VolumeCommand::Absolute(Volume::MIN),
    };

    match_next_token!(
        p,

        &Token::Number(v) | &Token::HexNumber(v) => {
            rv(v.saturating_mul(COARSE_VOLUME_MULTIPLIER.into()))
        },
        Token::PercentSign => {
            match next_token_number(p) {
                Some(v) => rv(v),
                None => {
                    p.add_error(pos, ValueError::NoRelativeVolume.into());
                    relative_volume(-i32::from(COARSE_VOLUME_MULTIPLIER))
                }
            }
        },
        #_ => {
            relative_volume(-i32::from(COARSE_VOLUME_MULTIPLIER))
        }
    )
}

fn parse_inc_volume_paren(pos: FilePos, p: &mut Parser) -> VolumeCommand {
    let rv = |v: u32| match i32::try_from(v) {
        Ok(v) => relative_volume(v),
        Err(_) => VolumeCommand::Absolute(Volume::MAX),
    };

    match_next_token!(
        p,

        &Token::Number(v) | &Token::HexNumber(v) => {
            rv(v.saturating_mul(COARSE_VOLUME_MULTIPLIER.into()))
        },
        Token::PercentSign => {
            match next_token_number(p) {
                Some(v) => rv(v),
                None => {
                    p.add_error(pos, ValueError::NoRelativeVolume.into());
                    relative_volume(COARSE_VOLUME_MULTIPLIER.into())
                }
            }
        },
        #_ => {
            relative_volume(COARSE_VOLUME_MULTIPLIER.into())
        }
    )
}

fn merge_pan_or_volume<'a>(
    pan: Option<PanCommand>,
    volume: Option<VolumeCommand>,
    p: &mut Parser<'a, '_>,
) -> Command<'a> {
    let mut pan = pan;
    let mut volume = volume;

    loop {
        let pos = p.peek_pos();

        match_next_token!(
            p,

            Token::CoarseVolume => {
                if let Some(v) = parse_coarse_volume_value(pos, p) {
                    volume = Some(merge_volumes_commands(volume, v));
                }
            },
            Token::FineVolume => {
                if let Some(v) = parse_fine_volume_value(pos, p) {
                    volume = Some(merge_volumes_commands(volume, v));
                }
            },
            Token::DecrementVolumeParentheses => {
                let v = parse_dec_volume_paren(pos, p);
                volume = Some(merge_volumes_commands(volume, v));
            },
            Token::IncrementVolumeParentheses => {
                let v = parse_inc_volume_paren(pos, p);
                volume = Some(merge_volumes_commands(volume, v));
            },
            Token::Pan => {
                if let Some(new_pan) = parse_pan_value(pos, p) {
                    pan = Some(merge_pan_commands(pan, new_pan));
                }
            },
            Token::PxPan => {
                if let Some(new_pan) = parse_px_pan_value(pos, p) {
                    pan = Some(merge_pan_commands(pan, new_pan));
                }
            },

            #_ => {
                if !merge_state_change(p) {
                    return Command::ChangePanAndOrVolume(pan, volume);
                }
            }
        )
    }
}

fn parse_coarse_volume_slide_amount(pos: FilePos, p: &mut Parser) -> Option<VolumeSlideAmount> {
    const MIN: i32 = -(MAX_COARSE_VOLUME as i32);
    const MAX: i32 = MAX_COARSE_VOLUME as i32;

    match_next_token!(
        p,

        &Token::RelativeNumber(n) => {
            match n {
                MIN..=MAX => {
                    match n {
                        MIN => Some(VolumeSlideAmount::MIN),
                        MAX => Some(VolumeSlideAmount::MAX),
                        n => match n.saturating_mul(COARSE_VOLUME_MULTIPLIER as i32).try_into() {
                            Ok(o) => Some(o),
                            Err(e) => {
                                p.add_error(pos, e.into());
                                None
                            }
                        }
                    }
                }
                _ => {
                    p.add_error(pos, ValueError::CoarseVolumeSlideOutOfRange(n).into());
                    None
                }
            }
        },
        &Token::Number(_) | &Token::HexNumber(_) => {
            p.add_error(pos, VolumeSlideAmount::MISSING_SIGN_ERROR.into());
            None
        },
        #_ => {
            p.add_error(pos, VolumeSlideAmount::MISSING_ERROR.into());
            None
        }
    )
}

fn _parse_volume_slide<'a>(
    pos: FilePos,
    p: &mut Parser<'a, '_>,
    amount: Option<VolumeSlideAmount>,
) -> Command<'a> {
    let ticks = parse_comma_ticks(pos, p);

    match (amount, ticks) {
        (Some(amount), Some(ticks)) => Command::VolumeSlide(amount, ticks),
        _ => Command::None,
    }
}

fn parse_coarse_volume_slide<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    let amount = parse_coarse_volume_slide_amount(pos, p);
    _parse_volume_slide(pos, p, amount)
}

fn parse_fine_volume_slide<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    let amount = parse_signed_newtype::<VolumeSlideAmount>(pos, p);
    _parse_volume_slide(pos, p, amount)
}

fn parse_coarse_tremolo_amplitude(pos: FilePos, p: &mut Parser) -> Option<TremoloAmplitude> {
    match_next_token!(
        p,

        &Token::Number(n) | &Token::HexNumber(n) => {
            match n {
                MIN_COARSE_TREMOLO_AMPLITUDE..=MAX_COARSE_TREMOLO_AMPLITUDE => {
                    match n {
                        MAX_COARSE_TREMOLO_AMPLITUDE => Some(TremoloAmplitude::MAX),
                        n => match n.saturating_mul(COARSE_VOLUME_MULTIPLIER.into()).try_into() {
                            Ok(o) => Some(o),
                            Err(e) => {
                                p.add_error(pos, e.into());
                                None
                            }
                        }
                    }
                }
                _ => {
                    p.add_error(pos, ValueError::CoarseTremoloAmplitudeOutOfRange(n).into());
                    None
                }
            }
        },
        #_ => {
            p.add_error(pos, TremoloAmplitude::MISSING_ERROR.into());
            None
        }
    )
}

fn _parse_tremolo<'a>(
    pos: FilePos,
    p: &mut Parser<'a, '_>,
    amount: Option<TremoloAmplitude>,
) -> Command<'a> {
    let ticks = parse_comma_ticks(pos, p);

    match (amount, ticks) {
        (Some(amplitude), Some(qwl)) => Command::Tremolo(amplitude, qwl),
        _ => Command::None,
    }
}

fn parse_fine_tremolo<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    let amplitude = parse_unsigned_newtype(pos, p);
    _parse_tremolo(pos, p, amplitude)
}

fn parse_coarse_tremolo<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    let amount = parse_coarse_tremolo_amplitude(pos, p);
    _parse_tremolo(pos, p, amount)
}

fn parse_pan_slide<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    let amount = parse_signed_newtype(pos, p);
    let ticks = parse_comma_ticks(pos, p);

    match (amount, ticks) {
        (Some(amount), Some(ticks)) => Command::PanSlide(amount, ticks),
        _ => Command::None,
    }
}

fn parse_panbrello<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    let amplitude = parse_unsigned_newtype(pos, p);
    let qwt = parse_comma_ticks(pos, p);

    match (amplitude, qwt) {
        (Some(amplitude), Some(qwl)) => Command::Panbrello(amplitude, qwl),
        _ => Command::None,
    }
}

// Assumes all ties have already been parsed.
// Requires the previously parsed token send a key-off event.
fn parse_rests_after_rest(p: &mut Parser) -> TickCounter {
    let mut ticks = TickCounter::default();

    if next_token_matches!(p, Token::Rest) {
        ticks += parse_tracked_length(p);

        loop {
            match_next_token!(
                p,

                Token::Rest => {
                    ticks += parse_tracked_length(p);
                },
                Token::Tie => {
                    ticks += parse_tracked_length(p);
                },
                #_ => {
                    if !merge_state_change(p) {
                        break;
                    }
                }
            )
        }
    }

    ticks
}

fn parse_rest_ticks_after_note(is_slur: bool, p: &mut Parser) -> RestTicksAfterNote {
    match (is_slur, p.is_keyoff_enabled()) {
        (false, true) => RestTicksAfterNote(parse_rests_after_rest(p)),
        (true, _) | (false, false) => {
            if next_token_matches!(p, Token::Rest) {
                RestTicksAfterNote(parse_tracked_length(p) + parse_ties(p))
            } else {
                RestTicksAfterNote(TickCounter::new(0))
            }
        }
    }
}

fn parse_ties(p: &mut Parser) -> TickCounter {
    let mut ticks = TickCounter::default();

    loop {
        match_next_token!(
            p,

            Token::Tie => {
                ticks += parse_tracked_length(p);
            },
            #_ => {
                if !merge_state_change(p) {
                    return ticks;
                }
            }
        )
    }
}

fn parse_ties_and_slur(p: &mut Parser) -> (TickCounter, bool) {
    let mut tie_length = TickCounter::new(0);

    loop {
        match_next_token!(
            p,

            Token::Tie => {
                tie_length += parse_tracked_length(p);
            },
            Token::Slur => {
                // Slur or tie
                if let Some(tc) = parse_tracked_optional_length(p) {
                    tie_length += tc;
                } else {
                    // slur
                    return (tie_length, true);
                }
            },

            #_ => {
                if !merge_state_change(p) {
                    return (tie_length, false);
                }
            }
        )
    }
}

fn parse_wait_length_and_ties(p: &mut Parser) -> TickCounter {
    let mut ticks = parse_tracked_length(p);

    // Merge `w` wait and `^` tie commands
    loop {
        match_next_token!(
            p,

            Token::Wait => {
                ticks += parse_tracked_length(p);
            },
            Token::Tie => {
                ticks += parse_tracked_length(p);
            },
            #_ => {
                if !merge_state_change(p) {
                    return ticks;
                }
            }
        )
    }
}

fn parse_wait<'a>(p: &mut Parser) -> Command<'a> {
    Command::Wait(parse_wait_length_and_ties(p))
}

fn parse_rest_lengths(p: &mut Parser) -> (TickCounter, TickCounter) {
    let ticks_until_keyoff = parse_tracked_length(p);
    let ticks_until_keyoff = ticks_until_keyoff + parse_ties(p);

    let ticks_after_keyoff = parse_rests_after_rest(p);

    (ticks_until_keyoff, ticks_after_keyoff)
}

fn parse_rest<'a>(p: &mut Parser<'a, '_>) -> Command<'a> {
    let (ticks_until_keyoff, ticks_after_keyoff) = parse_rest_lengths(p);

    Command::Rest {
        ticks_until_keyoff,
        ticks_after_keyoff,
    }
}

fn play_note<'a>(
    pos: FilePos,
    note: Note,
    length: TickCounter,
    p: &mut Parser<'a, '_>,
) -> Command<'a> {
    let (tie_length, is_slur) = parse_ties_and_slur(p);
    let length = length + tie_length;

    if !is_slur && length.value() <= KEY_OFF_TICK_DELAY {
        return invalid_token_error(p, pos, ChannelError::NoteIsTooShort);
    }

    let rest_after_note = parse_rest_ticks_after_note(is_slur, p);

    Command::PlayNote {
        note,
        length,
        is_slur,
        rest_after_note,
    }
}

fn pitch_to_note(pitch: MmlPitch, p: &Parser) -> Result<Note, ChannelError> {
    let s = p.state();

    match Note::from_mml_pitch(pitch, s.octave, &s.signature, s.semitone_offset) {
        Ok(n) => Ok(n),
        Err(e) => Err(e.into()),
    }
}

fn parse_pitch<'a>(pos: FilePos, pitch: MmlPitch, p: &mut Parser<'a, '_>) -> Command<'a> {
    match pitch_to_note(pitch, p) {
        Ok(note) => {
            let length = parse_tracked_length(p);
            play_note(pos, note, length, p)
        }
        Err(e) => {
            p.add_error(pos, e);

            // Output a rest (so tick-counter is correct)
            let length = parse_tracked_length(p);
            let (tie_length, _) = parse_ties_and_slur(p);
            let length = length + tie_length;
            Command::Rest {
                ticks_until_keyoff: length,
                ticks_after_keyoff: TickCounter::default(),
            }
        }
    }
}

fn parse_play_sample<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    let index = next_token_number(p).unwrap_or(0);

    let length = if next_token_matches!(p, Token::Comma) {
        parse_tracked_length(p)
    } else {
        p.increment_tick_offset(p.default_length());
        p.default_length()
    };

    match Note::from_note_id_u32(index) {
        Ok(note) => play_note(pos, note, length, p),
        Err(e) => {
            p.add_error(pos, e.into());

            // Output a rest (so tick-counter is correct)
            let (tie_length, _) = parse_ties_and_slur(p);
            Command::Rest {
                ticks_until_keyoff: length + tie_length,
                ticks_after_keyoff: TickCounter::new(0),
            }
        }
    }
}

fn parse_after_play_pitch<'a>(pitch: PlayPitchPitch, p: &mut Parser) -> Command<'a> {
    let p_length = parse_tracked_comma_length(p);

    let (tie_length, is_slur) = parse_ties_and_slur(p);
    let length = p_length + tie_length;

    let rest_after_note = parse_rest_ticks_after_note(is_slur, p);

    Command::PlayPitch {
        pitch,
        length,
        is_slur,
        rest_after_note,
    }
}

fn parse_play_pitch<'a>(pos: FilePos, p: &mut Parser) -> Command<'a> {
    let pitch = parse_unsigned_newtype(pos, p).unwrap_or(PlayPitchPitch::NATIVE);
    parse_after_play_pitch(pitch, p)
}

fn parse_play_pitch_sample_rate<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    let pitch = parse_play_pitch_sample_rate_value(pos, p);
    parse_after_play_pitch(pitch, p)
}

fn parse_play_pitch_frequency<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    let frequency = parse_unsigned_newtype(pos, p);
    let p_length = parse_tracked_comma_length(p);

    let (tie_length, is_slur) = parse_ties_and_slur(p);
    let length = p_length + tie_length;

    let rest_after_note = parse_rest_ticks_after_note(is_slur, p);

    match frequency {
        Some(frequency) => Command::PlayPitchFrequency {
            frequency,
            length,
            is_slur,
            rest_after_note,
        },
        None => {
            // Output a rest (so tick-counter is correct)
            Command::Rest {
                ticks_until_keyoff: p_length,
                ticks_after_keyoff: rest_after_note.0,
            }
        }
    }
}

fn parse_play_noise<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    let frequency = parse_unsigned_newtype(pos, p).unwrap_or(NoiseFrequency::MIN);
    let p_length = parse_tracked_comma_length(p);

    let (tie_length, is_slur) = parse_ties_and_slur(p);
    let length = p_length + tie_length;

    let rest_after_note = parse_rest_ticks_after_note(is_slur, p);

    Command::PlayNoise {
        frequency,
        length,
        is_slur,
        rest_after_note,
    }
}

fn parse_play_midi_note_number<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    let length = p.default_length();
    p.increment_tick_offset(length);

    let note = match parse_unsigned_newtype::<MidiNote>(pos, p) {
        Some(n) => match n.try_into() {
            Ok(n) => Some(n),
            Err(e) => {
                let e: ValueError = e;
                p.add_error(pos, e.into());
                None
            }
        },
        None => None,
    };

    match note {
        Some(note) => play_note(pos, note, length, p),
        None => {
            // Output a rest (so tick-counter is correct)
            let (tie_length, _) = parse_ties_and_slur(p);
            Command::Rest {
                ticks_until_keyoff: length + tie_length,
                ticks_after_keyoff: TickCounter::new(0),
            }
        }
    }
}

fn parse_portamento<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    let notes = parse_portamento_pitches(pos, p);

    if matches!(notes, Err(PortamentoPitchError::MissingEnd)) {
        return Command::None;
    }

    let portamento_length = parse_tracked_length(p);

    let mut slide_length = portamento_length;
    let mut delay_length = TickCounter::new(0);
    let mut speed_override = None;

    if next_token_matches!(p, Token::Comma) {
        let dt_pos = p.peek_pos();
        if let Some(dt) = parse_untracked_optional_length(p) {
            if dt < slide_length {
                slide_length = TickCounter::new(slide_length.value() - dt.value());
                delay_length = dt
            } else {
                p.add_error(dt_pos, ChannelError::InvalidPortamentoDelay);
            }
        }
        if next_token_matches!(p, Token::Comma) {
            speed_override = parse_unsigned_newtype(p.peek_pos(), p);
        }
    }

    let (tie_length, is_slur) = parse_ties_and_slur(p);

    let rest_after_note = parse_rest_ticks_after_note(is_slur, p);

    match notes {
        Ok((note1, note2)) => Command::Portamento {
            note1,
            note2,
            is_slur,
            speed_override,
            delay_length,
            slide_length,
            tie_length,
            rest_after_note,
        },
        Err(_) => {
            // Output a rest (so tick-counter is correct)
            Command::Rest {
                ticks_until_keyoff: portamento_length,
                ticks_after_keyoff: rest_after_note.0,
            }
        }
    }
}

fn parse_broken_chord<'a>(p: &mut Parser<'a, '_>) -> Command<'a> {
    let (notes, end_pos) = match parse_broken_chord_pitches(p) {
        Some(s) => s,
        None => return Command::None,
    };

    let mut note_length_pos = end_pos;

    let total_length = parse_tracked_length(p);
    let mut tie = true;
    let mut note_length = None;

    if next_token_matches!(p, Token::Comma) {
        note_length_pos = p.peek_pos();
        note_length = parse_untracked_optional_length(p);

        if next_token_matches!(p, Token::Comma) {
            let tie_pos = p.peek_pos();
            match_next_token!(
                p,
                Token::Number(0) | Token::HexNumber(0) => tie = false,
                Token::Number(1) | Token::HexNumber(1) => tie = true,
                Token::Number(_) | Token::HexNumber(_) => p.add_error(tie_pos, ValueError::InvalidMmlBool.into()),
                #_ => p.add_error(tie_pos, ValueError::NoBool.into())
            )
        }
    }

    let note_length = match note_length {
        Some(nl) => match PlayNoteTicks::try_from_is_slur(nl.value(), tie) {
            Ok(t) => t,
            Err(e) => {
                p.add_error(note_length_pos, e.into());
                PlayNoteTicks::min_for_is_slur(tie)
            }
        },
        None => PlayNoteTicks::min_for_is_slur(tie),
    };

    let slur_last_note = next_token_matches!(p, Token::Slur);

    Command::BrokenChord {
        notes,
        total_length,
        note_length,
        slur_last_note,
    }
}

fn parse_optional_vibrato_delay(p: &mut Parser) -> VibratoDelayTicks {
    parse_optional_comma_ticks(p).unwrap_or(VibratoDelayTicks::new(0))
}

fn parse_mp_vibrato(pos: FilePos, p: &mut Parser) -> Option<MpVibrato> {
    match_next_token!(
        p,

        &Token::Number(0) | &Token::HexNumber(0) => {
            // Disable MP Vibrato
            None
        },
        &Token::Number(depth_in_cents) => {
            parse_comma_ticks(pos, p).map(|qwt| MpVibrato {
                depth_in_cents,
                quarter_wavelength_ticks: qwt,
                delay: parse_optional_vibrato_delay(p),
            })
        },
        #_ => {
            p.add_error(pos, ValueError::NoMpDepth.into());
            None
        }
    )
}

fn parse_manual_vibrato(pos: FilePos, p: &mut Parser) -> Option<ManualVibrato> {
    let pitch_offset_per_tick: VibratoPitchOffsetPerTick = parse_unsigned_newtype(pos, p)?;

    if pitch_offset_per_tick.as_u8() == 0 {
        // Disable MP Vibrato
        None
    } else {
        parse_comma_ticks(pos, p).map(|qwt| ManualVibrato {
            pitch_offset_per_tick,
            quarter_wavelength_ticks: qwt,
            delay: parse_optional_vibrato_delay(p),
        })
    }
}

fn parse_set_adsr<'a>(pos: FilePos, p: &mut Parser<'a, '_>) -> Command<'a> {
    let mut values = [0; 4];

    values[0] = match next_token_number(p) {
        Some(n) => n,
        None => {
            p.add_error(pos, ValueError::AdsrNotFourValues.into());
            return Command::None;
        }
    };

    for v in &mut values[1..] {
        if !next_token_matches!(p, Token::Comma) {
            p.add_error(pos, ValueError::AdsrNotFourValues.into());
            return Command::None;
        }

        *v = match next_token_number(p) {
            Some(n) => n,
            None => {
                p.add_error(pos, ValueError::AdsrNotFourValues.into());
                return Command::None;
            }
        };
    }

    match values.try_into() {
        Ok(adsr) => Command::SetAdsr(adsr),
        Err(e) => {
            p.add_error(pos, e.into());
            Command::None
        }
    }
}

fn parse_set_gain<'a>(pos: FilePos, mode: GainMode, p: &mut Parser<'a, '_>) -> Command<'a> {
    match next_token_number(p) {
        Some(v) => match Gain::from_mode_and_value(mode, v) {
            Ok(gain) => Command::SetGain(gain),
            Err(e) => invalid_token_error(p, pos, e.into()),
        },
        None => invalid_token_error(p, pos, ValueError::NoGain.into()),
    }
}

fn parse_temp_gain<'a>(pos: FilePos, mode: GainMode, p: &mut Parser<'a, '_>) -> Command<'a> {
    let mut temp_gain = match next_token_number(p) {
        Some(v) => match TempGain::try_from_mode_and_value(mode, v) {
            Ok(g) => Some(g),
            Err(e) => return invalid_token_error(p, pos, e.into()),
        },
        None => None,
    };

    loop {
        match_next_token!(
            p,

            &Token::TempGain(mode) => {
                temp_gain = match next_token_number(p) {
                    Some(v) => match TempGain::try_from_mode_and_value(mode, v) {
                        Ok(g) => Some(g),
                        Err(e) => return invalid_token_error(p, pos, e.into()),
                    },
                    // Temp gain is unchanged
                    None => temp_gain,
                };
            },

            Token::Rest => {
                let (ticks_until_keyoff, ticks_after_keyoff) = parse_rest_lengths(p);

                return Command::TempGainAndRest{temp_gain, ticks_until_keyoff, ticks_after_keyoff};
            },

            Token::Wait => {
                let length = parse_wait_length_and_ties(p);

                return Command::TempGainAndWait(temp_gain, length);
            },

            #_ => {
                if !merge_state_change(p) {
                    return Command::TempGain(temp_gain);
                }
            }
        )
    }
}

fn parse_pitch_mod<'a>(pos: FilePos, p: &mut Parser) -> Command<'a> {
    match_next_token!(p,
        Token::Number(0) | Token::HexNumber(0) => Command::DisablePitchMod,
        Token::Number(1) | Token::HexNumber(1) => Command::EnablePitchMod,
        Token::Number(_) | Token::HexNumber(_) => invalid_token_error(p, pos, ValueError::InvalidMmlBool.into()),
        #_ => Command::EnablePitchMod
    )
}

fn parse_echo<'a>(pos: FilePos, p: &mut Parser) -> Command<'a> {
    match_next_token!(p,
        Token::Number(0) | Token::HexNumber(0) => Command::SetEcho(false),
        Token::Number(1) | Token::HexNumber(1) => Command::SetEcho(true),
        Token::Number(_) | Token::HexNumber(_) => invalid_token_error(p, pos, ValueError::InvalidMmlBool.into()),
        #_ => Command::SetEcho(true)
    )
}

fn parse_keyoff<'a>(pos: FilePos, p: &mut Parser) -> Command<'a> {
    let keyoff_enabled = match_next_token!(p,
        Token::Number(0) | Token::HexNumber(0) => false,
        Token::Number(1) | Token::HexNumber(1) => true,
        Token::Number(_) | Token::HexNumber(_) => {
            p.add_error(pos, ValueError::InvalidMmlBool.into());
            true
        },
        #_ => true
    );

    p.set_keyoff_enabled(keyoff_enabled);
    p.set_state(State {
        keyoff_enabled,
        ..p.state().clone()
    });

    Command::SetKeyOff(keyoff_enabled)
}

fn parse_set_instrument<'a>(pos: FilePos, id: IdentifierStr, p: &mut Parser) -> Command<'a> {
    match p.instruments_map().get(&id) {
        Some(inst) => Command::SetInstrument(inst.instrument_id, inst.envelope),
        None => invalid_token_error(
            p,
            pos,
            ChannelError::CannotFindInstrument(id.as_str().to_owned()),
        ),
    }
}

fn parse_set_instrument_hint<'a>(
    pos: FilePos,
    id: IdentifierStr,
    p: &mut Parser<'a, '_>,
) -> Command<'a> {
    match p.instruments_map().get(&id) {
        Some(inst) => Command::SetSubroutineInstrumentHint(inst.instrument_id, inst.envelope),
        None => invalid_token_error(
            p,
            pos,
            ChannelError::CannotFindInstrument(id.as_str().to_owned()),
        ),
    }
}

fn parse_call_subroutine<'a>(
    pos: FilePos,
    id: IdentifierStr,
    d: SubroutineCallType,
    p: &mut Parser<'a, '_>,
) -> Command<'a> {
    match p.find_subroutine(id.as_str()) {
        Some(index) => Command::CallSubroutine(index, d),
        None => match p.channel_id() {
            ChannelId::Channel(_) | ChannelId::Subroutine(_) | ChannelId::SoundEffect => {
                invalid_token_error(
                    p,
                    pos,
                    ChannelError::CannotFindSubroutine(id.as_str().to_owned()),
                )
            }
            ChannelId::MmlPrefix => {
                invalid_token_error(p, pos, ChannelError::CannotCallSubroutineInAnMmlPrefix)
            }
        },
    }
}

fn echo_volume_or_min(n: u32, pos: FilePos, p: &mut Parser) -> EchoVolume {
    match n.try_into() {
        Ok(o) => o,
        Err(e) => {
            p.add_error(pos, e.into());
            EchoVolume::MIN
        }
    }
}

fn relative_echo_volume_or_min(n: i32, pos: FilePos, p: &mut Parser) -> RelativeEchoVolume {
    match n.try_into() {
        Ok(o) => o,
        Err(e) => {
            p.add_error(pos, e.into());
            RelativeEchoVolume::MIN
        }
    }
}

fn parse_evol<'a>(pos: FilePos, p: &mut Parser) -> Command<'a> {
    let first_pos = p.peek_pos();

    match_next_token!(p,
        &Token::Number(v1) | &Token::HexNumber(v1) => {
            let v1 = echo_volume_or_min(v1, first_pos, p);
            if next_token_matches!(p, Token::Comma) {
                let second_pos = p.peek_pos();
                match_next_token!(p,
                    &Token::Number(v2) | &Token::HexNumber(v2) => {
                        let v2 = echo_volume_or_min(v2, second_pos, p);
                        Command::SetStereoEchoVolume(v1, v2)
                    },
                    &Token::RelativeNumber(_) => {
                        invalid_token_error(p, pos, ChannelError::SetAndRelativeStereoEchoVolume)
                    },
                    #_ => {
                        invalid_token_error(p, pos, ValueError::NoEchoVolume.into())
                    }
                )
            }
            else {
                Command::SetEchoVolume(v1)
            }
        },
        &Token::RelativeNumber(r1) => {
            let r1 = relative_echo_volume_or_min(r1, first_pos, p);
            if next_token_matches!(p, Token::Comma) {
                let second_pos = p.peek_pos();
                match_next_token!(p,
                    &Token::RelativeNumber(r2) => {
                        let r2 = relative_echo_volume_or_min(r2, second_pos, p);
                        Command::RelativeStereoEchoVolume(r1, r2)
                    },
                    &Token::Number(_) | &Token::HexNumber(_) => {
                        invalid_token_error(p, pos, ChannelError::RelativeAndSetStereoEchoVolume)
                    },
                    #_ => {
                        invalid_token_error(p, pos, ValueError::NoEchoVolume.into())
                    }
                )
            }
            else {
                Command::RelativeEchoVolume(r1)
            }
        },
        #_ => invalid_token_error(p, pos, ChannelError::NoEvolValue)
    )
}

fn parse_efb<'a>(pos: FilePos, p: &mut Parser) -> Command<'a> {
    Command::SetEchoFeedback(parse_i8wh_newtype(pos, p))
}

fn parse_efb_plus<'a>(pos: FilePos, p: &mut Parser) -> Command<'a> {
    let value_pos = p.peek_pos();

    let rel: RelativeEchoFeedback = match_next_token!(p,
        &Token::Number(n) | &Token::HexNumber(n) => {
            match i32::try_from(n) {
                Ok(i) => match i.try_into() {
                    Ok(i) => i,
                    Err(e) => {
                        p.add_error(value_pos, e.into());
                        RelativeEchoFeedback::ZERO
                    }
                },
                Err(_) => {
                    p.add_error(value_pos, ValueError::RelativeEchoFeedbackOutOfRangeU32(n).into());
                    RelativeEchoFeedback::ZERO
                }
            }
        },
        #_ => {
            p.add_error(pos, ValueError::NoRelativeEchoFeedback.into());
            RelativeEchoFeedback::ZERO
        }
    );

    let pos = p.peek_pos();

    if next_token_matches!(p, Token::Comma) {
        let limit = parse_i8wh_newtype(pos, p);
        Command::RelativeEchoFeedbackWithLimit(rel, limit)
    } else {
        Command::RelativeEchoFeedback(rel)
    }
}

fn parse_efb_minus<'a>(pos: FilePos, p: &mut Parser) -> Command<'a> {
    let value_pos = p.peek_pos();

    let rel: RelativeEchoFeedback = match_next_token!(p,
        &Token::Number(n) | &Token::HexNumber(n) => {
            match i32::try_from(n) {
                // `-i`` is safe, `-i32::MAX` is in bounds
                Ok(i) => match (-i).try_into() {
                    Ok(i) => i,
                    Err(e) => {
                        p.add_error(value_pos, e.into());
                        RelativeEchoFeedback::ZERO
                    }
                },
                Err(_) => {
                    p.add_error(value_pos, ValueError::RelativeEchoFeedbackOutOfRangeU32(n).into());
                    RelativeEchoFeedback::ZERO
                }
            }
        },
        #_ => {
            p.add_error(pos, ValueError::NoRelativeEchoFeedback.into());
            RelativeEchoFeedback::ZERO
        }
    );

    let pos = p.peek_pos();

    if next_token_matches!(p, Token::Comma) {
        let limit = parse_i8wh_newtype(pos, p);
        Command::RelativeEchoFeedbackWithLimit(rel, limit)
    } else {
        Command::RelativeEchoFeedback(rel)
    }
}

fn parse_ftap<'a>(pos: FilePos, p: &mut Parser) -> Command<'a> {
    let tap = parse_unsigned_newtype(pos, p).unwrap_or(FirTap::MIN);

    if next_token_matches!(p, Token::Comma) {
        let value_pos = p.peek_pos();
        Command::SetFirTap(tap, parse_i8wh_newtype(value_pos, p))
    } else {
        invalid_token_error(p, p.peek_pos(), ValueError::NoCommaFirCoefficient.into())
    }
}

fn parse_ftap_plus<'a>(pos: FilePos, p: &mut Parser) -> Command<'a> {
    let tap = parse_unsigned_newtype(pos, p).unwrap_or(FirTap::MIN);

    if next_token_matches!(p, Token::Comma) {
        let value_pos = p.peek_pos();

        let rel: RelativeFirCoefficient = match_next_token!(p,
            &Token::Number(n) | &Token::HexNumber(n) => {
                match i32::try_from(n) {
                    Ok(i) => match (i).try_into() {
                        Ok(i) => i,
                        Err(e) => {
                            p.add_error(value_pos, e.into());
                            RelativeFirCoefficient::ZERO
                        }
                    },
                    Err(_) => {
                        p.add_error(value_pos, ValueError::RelativeFirCoefficientOutOfRangeU32(n).into());
                        RelativeFirCoefficient::ZERO
                    }
                }
            },
            #_ => {
                p.add_error(value_pos, ValueError::NoRelativeFirCoefficient.into());
                RelativeFirCoefficient::ZERO
            }
        );

        let pos = p.peek_pos();

        if next_token_matches!(p, Token::Comma) {
            let limit = parse_i8wh_newtype(pos, p);
            Command::AdjustFirTapWithLimit(tap, rel, limit)
        } else {
            Command::AdjustFirTap(tap, rel)
        }
    } else {
        invalid_token_error(
            p,
            p.peek_pos(),
            ValueError::NoCommaRelativeFirCoefficient.into(),
        )
    }
}

fn parse_ftap_minus<'a>(pos: FilePos, p: &mut Parser) -> Command<'a> {
    let tap = parse_unsigned_newtype(pos, p).unwrap_or(FirTap::MIN);

    if next_token_matches!(p, Token::Comma) {
        let value_pos = p.peek_pos();

        let rel: RelativeFirCoefficient = match_next_token!(p,
            &Token::Number(n) | &Token::HexNumber(n) => {
                match i32::try_from(n) {
                    // `-i`` is safe, `-i32::MAX` is in bounds
                    Ok(i) => match (-i).try_into() {
                        Ok(i) => i,
                        Err(e) => {
                            p.add_error(value_pos, e.into());
                            RelativeFirCoefficient::ZERO
                        }
                    },
                    Err(_) => {
                        p.add_error(value_pos, ValueError::RelativeFirCoefficientOutOfRangeU32(n).into());
                        RelativeFirCoefficient::ZERO
                    }
                }
            },
            #_ => {
                p.add_error(value_pos, ValueError::NoRelativeFirCoefficient.into());
                RelativeFirCoefficient::ZERO
            }
        );

        let pos = p.peek_pos();

        if next_token_matches!(p, Token::Comma) {
            let limit = parse_i8wh_newtype(pos, p);
            Command::AdjustFirTapWithLimit(tap, rel, limit)
        } else {
            Command::AdjustFirTap(tap, rel)
        }
    } else {
        invalid_token_error(
            p,
            p.peek_pos(),
            ValueError::NoCommaRelativeFirCoefficient.into(),
        )
    }
}

fn parse_fir_filter<'a>(fir_pos: FilePos, p: &mut Parser) -> Command<'a> {
    if !next_token_matches!(p, Token::StartPortamento) {
        return invalid_token_error(p, fir_pos, ChannelError::NoBraceAfterFirFilter);
    }

    let mut filter = [FirCoefficient::ZERO; FIR_FILTER_SIZE];
    let mut count = 0;

    loop {
        let (pos, token) = p.peek_and_next();

        match token {
            Token::EndPortamento => {
                if count == filter.len() {
                    return Command::SetFirFilter(filter);
                } else {
                    return invalid_token_error(
                        p,
                        fir_pos,
                        ChannelError::InvalidNumberOfFirCoefficients(count),
                    );
                }
            }

            Token::EndBrokenChord | Token::End => {
                return invalid_token_error(p, fir_pos, ChannelError::MissingEndFirFilter)
            }
            Token::NewLine(r) => {
                p.add_error(pos, ChannelError::MissingEndFirFilter);
                p.process_new_line(r);
                return Command::None;
            }

            Token::Number(n) => {
                match n.try_into() {
                    Ok(value) => {
                        if let Some(f) = filter.get_mut(count) {
                            *f = value;
                        }
                    }
                    Err(e) => p.add_error(pos, e.into()),
                }
                count += 1;
            }
            Token::HexNumber(h) => {
                match FirCoefficient::try_from_hex_byte(h) {
                    Ok(value) => {
                        if let Some(f) = filter.get_mut(count) {
                            *f = value;
                        }
                    }
                    Err(e) => p.add_error(pos, e.into()),
                }
                count += 1;
            }
            Token::RelativeNumber(n) => {
                match n.try_into() {
                    Ok(value) => {
                        if let Some(f) = filter.get_mut(count) {
                            *f = value;
                        }
                    }
                    Err(e) => p.add_error(pos, e.into()),
                }
                count += 1;
            }

            _ => {
                p.add_error(pos, ChannelError::UnknownTokenInFirFilter);
            }
        }
    }
}

fn parse_set_echo_delay<'a>(pos: FilePos, p: &mut Parser) -> Command<'a> {
    match parse_unsigned_newtype(pos, p) {
        Some(l) => Command::SetEchoDelay(l),
        None => Command::None,
    }
}

fn parse_bytecode_asm<'a>(pos: FilePos, asm: &'a str, p: &mut Parser<'a, '_>) -> Command<'a> {
    match asm.split_once(|c: char| c.is_ascii_whitespace()) {
        Some((bytecode_assembler::CALL_SUBROUTINE, arg)) => parse_call_subroutine(
            pos,
            IdentifierStr::from_str(arg.trim_start()),
            SubroutineCallType::Asm,
            p,
        ),
        Some((bytecode_assembler::CALL_SUBROUTINE_AND_DISABLE_VIBRATO, arg)) => {
            parse_call_subroutine(
                pos,
                IdentifierStr::from_str(arg.trim_start()),
                SubroutineCallType::AsmDisableVibrato,
                p,
            )
        }
        Some((bytecode_assembler::START_LOOP, arg)) => {
            match bytecode_assembler::parse_uvnt(arg.trim_start()) {
                Ok(a) => Command::StartLoop(Some(a), Default::default()),
                Err(e) => {
                    p.add_error(pos, e);
                    Command::None
                }
            }
        }
        Some((bytecode_assembler::END_LOOP, arg)) => {
            match bytecode_assembler::parse_uvnt(arg.trim_start()) {
                Ok(a) => Command::EndLoop(Some(a), Default::default()),
                Err(e) => {
                    p.add_error(pos, e);
                    Command::None
                }
            }
        }
        Some((bytecode_assembler::SET_TRANSPOSE, arg)) => {
            match bytecode_assembler::parse_svnt_allow_zero(arg.trim_start()) {
                Ok(t) => Command::SetTranspose(t),
                Err(e) => {
                    p.add_error(pos, e);
                    Command::None
                }
            }
        }
        Some((bytecode_assembler::ADJUST_TRANSPOSE, arg)) => {
            match bytecode_assembler::parse_svnt(arg.trim_start()) {
                Ok(t) => Command::AdjustTranspose(t),
                Err(e) => {
                    p.add_error(pos, e);
                    Command::None
                }
            }
        }
        Some(_) => Command::BytecodeAsm(asm),

        // Instructions with no arguments
        None => match asm {
            bytecode_assembler::START_LOOP => Command::StartLoop(None, Default::default()),
            bytecode_assembler::SKIP_LAST_LOOP => Command::SkipLastLoop,
            bytecode_assembler::END_LOOP => Command::EndLoop(None, Default::default()),
            bytecode_assembler::DISABLE_TRANSPOSE => Command::SetTranspose(Transpose::ZERO),
            _ => Command::BytecodeAsm(asm),
        },
    }
}

fn invalid_token_error<'a>(p: &mut Parser, pos: FilePos, e: ChannelError) -> Command<'a> {
    p.add_error(pos, e);
    Command::None
}

fn parse_token<'a>(pos: FilePos, token: Token<'a>, p: &mut Parser<'a, '_>) -> Command<'a> {
    p.reset_tick_offset();

    match token {
        Token::End => Command::None,

        Token::NewLine(r) => {
            p.process_new_line(r);
            Command::None
        }

        Token::Pitch(pitch) => parse_pitch(pos, pitch, p),
        Token::PlayPitch => parse_play_pitch(pos, p),
        Token::PlayPitchSampleRate => parse_play_pitch_sample_rate(pos, p),
        Token::PlayPitchFrequency => parse_play_pitch_frequency(pos, p),
        Token::PlayNoise => parse_play_noise(pos, p),
        Token::PlaySample => parse_play_sample(pos, p),
        Token::PlayMidiNoteNumber => parse_play_midi_note_number(pos, p),
        Token::Rest => parse_rest(p),
        Token::Wait => parse_wait(p),
        Token::StartPortamento => parse_portamento(pos, p),
        Token::StartBrokenChord => parse_broken_chord(p),

        Token::DisableNoise => Command::DisableNoise,

        Token::MpVibrato => Command::SetMpVibrato(parse_mp_vibrato(pos, p)),
        Token::ManualVibrato => Command::SetManualVibrato(parse_manual_vibrato(pos, p)),

        Token::SetAdsr => parse_set_adsr(pos, p),
        Token::SetGain(mode) => parse_set_gain(pos, mode, p),
        Token::TempGain(mode) => parse_temp_gain(pos, mode, p),

        Token::SetInstrument(id) => parse_set_instrument(pos, id, p),
        Token::SetSubroutineInstrumentHint(id) => parse_set_instrument_hint(pos, id, p),
        Token::CallSubroutine(id) => parse_call_subroutine(pos, id, SubroutineCallType::Mml, p),

        Token::StartLoop => Command::StartLoop(None, Default::default()),
        Token::SkipLastLoop => Command::SkipLastLoop,
        Token::EndLoop => {
            let lc = parse_unsigned_newtype(pos, p).unwrap_or(LoopCount::MIN);
            Command::EndLoop(Some(lc), Default::default())
        }

        Token::SetLoopPoint => Command::SetLoopPoint(Default::default()),

        Token::PitchMod => parse_pitch_mod(pos, p),
        Token::Echo => parse_echo(pos, p),
        Token::Keyoff => parse_keyoff(pos, p),

        Token::SetSongTempo => match parse_unsigned_newtype(pos, p) {
            Some(t) => Command::SetSongTempo(t),
            None => Command::None,
        },
        Token::SetSongTickClock => match parse_unsigned_newtype(pos, p) {
            Some(t) => Command::SetSongTickClock(t),
            None => Command::None,
        },

        Token::CoarseVolume => {
            let v = parse_coarse_volume_value(pos, p);
            merge_pan_or_volume(None, v, p)
        }
        Token::FineVolume => {
            let v = parse_fine_volume_value(pos, p);
            merge_pan_or_volume(None, v, p)
        }
        Token::DecrementVolumeParentheses => {
            let v = parse_dec_volume_paren(pos, p);
            merge_pan_or_volume(None, Some(v), p)
        }
        Token::IncrementVolumeParentheses => {
            let v = parse_inc_volume_paren(pos, p);
            merge_pan_or_volume(None, Some(v), p)
        }
        Token::Pan => {
            let pan = parse_pan_value(pos, p);
            merge_pan_or_volume(pan, None, p)
        }
        Token::PxPan => {
            let pan = parse_px_pan_value(pos, p);
            merge_pan_or_volume(pan, None, p)
        }

        Token::SetChannelInvert(flags) => Command::SetChannelInvert(flags),

        Token::CoarseVolumeSlide => parse_coarse_volume_slide(pos, p),
        Token::FineVolumeSlide => parse_fine_volume_slide(pos, p),
        Token::CoarseTremolo => parse_coarse_tremolo(pos, p),
        Token::FineTremolo => parse_fine_tremolo(pos, p),

        Token::PanSlide => parse_pan_slide(pos, p),
        Token::Panbrello => parse_panbrello(pos, p),

        Token::Quantize => parse_quantize(pos, p),
        Token::EarlyRelease => parse_set_early_release(pos, p),

        Token::Transpose => parse_transpose(pos, p),
        Token::RelativeTranspose => parse_relative_transpose(pos, p),

        Token::DetuneOrGainModeD => parse_detune(pos, p),
        Token::DetuneCents => parse_detune_cents(pos, p),

        Token::SetDefaultLength => {
            parse_set_default_length(pos, p);
            Command::None
        }
        Token::SetOctave => {
            parse_set_octave(pos, p);
            Command::None
        }
        Token::IncrementOctave => {
            parse_increment_octave(p);
            Command::None
        }
        Token::DecrementOctave => {
            parse_decrement_octave(p);
            Command::None
        }

        Token::ChannelTranspose => {
            parse_channel_transpose(pos, p);
            Command::None
        }
        Token::RelativeChannelTranspose => {
            parse_relative_channel_transpose(pos, p);
            Command::None
        }
        Token::KeySignature(s) => {
            parse_key_signature(pos, p, s);
            Command::None
        }

        Token::ChangeWholeNoteLength => {
            parse_change_whole_note_length(pos, p);
            Command::None
        }
        Token::Divider => Command::None,

        Token::Evol => parse_evol(pos, p),
        Token::Efb => parse_efb(pos, p),
        Token::EfbPlus => parse_efb_plus(pos, p),
        Token::EfbMinus => parse_efb_minus(pos, p),
        Token::Fir => parse_fir_filter(pos, p),
        Token::Ftap => parse_ftap(pos, p),
        Token::FtapPlus => parse_ftap_plus(pos, p),
        Token::FtapMinus => parse_ftap_minus(pos, p),
        Token::SetEchoInvert(flags) => Command::SetEchoInvert(flags),
        Token::SetEchoDelay => parse_set_echo_delay(pos, p),

        Token::StartBytecodeAsm => Command::StartBytecodeAsm,
        Token::BytecodeAsm(asm) => parse_bytecode_asm(pos, asm, p),
        Token::EndBytecodeAsm => Command::EndBytecodeAsm,

        Token::EndPortamento => invalid_token_error(p, pos, ChannelError::NoStartPortamento),
        Token::EndBrokenChord => invalid_token_error(p, pos, ChannelError::NoStartBrokenChord),

        Token::Tie => invalid_token_error(p, pos, ChannelError::MissingNoteBeforeTie),
        Token::Slur => invalid_token_error(p, pos, ChannelError::MissingNoteBeforeSlur),
        Token::Comma => invalid_token_error(p, pos, ChannelError::CannotParseComma),
        Token::Dot => invalid_token_error(p, pos, ChannelError::CannotParseDot),
        Token::PercentSign => invalid_token_error(p, pos, ChannelError::CannotParsePercentSign),
        Token::Number(_) | Token::HexNumber(_) => {
            invalid_token_error(p, pos, ChannelError::UnexpectedNumber)
        }
        Token::RelativeNumber(_) => invalid_token_error(p, pos, ChannelError::UnexpectedNumber),

        Token::GainModeB | Token::GainModeF | Token::GainModeI => {
            invalid_token_error(p, pos, ChannelError::CannotParseGainMode)
        }

        Token::Error(e) => invalid_token_error(p, pos, e),
    }
}

pub(crate) fn parse_mml_tokens<'a>(
    channel: ChannelId,
    tokens: MmlTokens<'a>,
    instruments_map: &HashMap<IdentifierStr, &MmlInstrument>,
    subroutines: &dyn SubroutineNameMap,
    settings: &GlobalSettings,
    cursor_tracking: &mut CursorTracker,
) -> (ChannelCommands<'a>, Vec<ErrorWithPos<ChannelError>>) {
    let mut commands = Vec::with_capacity(tokens.len() / 2);

    let mut p = Parser::new(
        channel,
        tokens,
        instruments_map,
        subroutines,
        settings,
        cursor_tracking,
    );

    let end_pos = loop {
        let (pos, token) = p.peek_and_next();

        match token {
            Token::End => break pos,
            t => commands.push(CommandWithPos::new(
                parse_token(pos, t, &mut p),
                p.file_pos_range_from(pos),
                p.command_end_char_index(),
            )),
        }
    };

    let errors = p.finalize();

    (ChannelCommands { commands, end_pos }, errors)
}
