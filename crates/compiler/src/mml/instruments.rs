//! MML instrument parser

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::{Identifier, IdentifierStr};

use crate::bytecode::InstrumentId;
use crate::data;
use crate::data::UniqueNamesList;
use crate::envelope::{Adsr, Envelope, Gain};
use crate::errors::{ErrorWithPos, MmlLineError};
use crate::file_pos::{FilePosRange, Line};
use crate::notes::Note;

use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum EnvelopeOverride {
    None,
    Adsr(Adsr),
    Gain(Gain),
}

#[derive(Debug)]
pub struct MmlInstrument {
    pub(crate) identifier: Identifier,

    pub(crate) file_range: FilePosRange,

    pub(crate) instrument_id: InstrumentId,

    pub(crate) first_note: Note,
    pub(crate) last_note: Note,

    pub(crate) envelope_override: EnvelopeOverride,
}

fn parse_instrument(
    id: Identifier,
    line: &Line,
    inst_map: &UniqueNamesList<data::Instrument>,
) -> Result<MmlInstrument, ErrorWithPos<MmlLineError>> {
    if line.text.is_empty() {
        return Err(ErrorWithPos(line.range(), MmlLineError::NoInstrument));
    }

    let (inst_name, args) = match line.split_once() {
        Some((n, a)) => (n, Some(a)),
        None => (line.text, None),
    };

    let inst_name_err = |e| Err(ErrorWithPos(line.position.to_range_str_len(inst_name), e));

    let mut envelope_override = EnvelopeOverride::None;

    if let Some(args) = args {
        let arg = args.text;
        let arg_range = args.range();
        let arg_err = |e| Err(ErrorWithPos(arg_range, e));

        match Envelope::try_from_envelope_str(arg) {
            Ok(Envelope::Adsr(a)) => {
                envelope_override = EnvelopeOverride::Adsr(a);
            }
            Ok(Envelope::Gain(g)) => {
                envelope_override = EnvelopeOverride::Gain(g);
            }
            Err(e) => {
                return arg_err(MmlLineError::ValueError(e));
            }
        }
    }

    let (instrument_id, inst) = match inst_map.get_with_index(inst_name) {
        Some((inst_id, inst)) => (inst_id, inst),
        None => {
            return inst_name_err(MmlLineError::CannotFindInstrument(inst_name.to_owned()));
        }
    };

    match envelope_override {
        EnvelopeOverride::None => {}
        EnvelopeOverride::Adsr(adsr) => {
            if inst.envelope == Envelope::Adsr(adsr) {
                envelope_override = EnvelopeOverride::None;
            }
        }
        EnvelopeOverride::Gain(gain) => {
            if inst.envelope == Envelope::Gain(gain) {
                envelope_override = EnvelopeOverride::None;
            }
        }
    }

    let instrument_id = match InstrumentId::try_from(instrument_id) {
        Ok(i) => i,
        Err(e) => return inst_name_err(e.into()),
    };

    Ok(MmlInstrument {
        identifier: id,
        file_range: line.range(),
        instrument_id,
        first_note: Note::first_note_for_octave(inst.first_octave),
        last_note: Note::last_note_for_octave(inst.last_octave),
        envelope_override,
    })
}

pub fn parse_instruments(
    instrument_lines: Vec<(Identifier, Line)>,
    inst_map: &UniqueNamesList<data::Instrument>,
) -> (Vec<MmlInstrument>, Vec<ErrorWithPos<MmlLineError>>) {
    let mut out = Vec::with_capacity(instrument_lines.len());
    let mut errors = Vec::new();

    for (id, line) in instrument_lines {
        match parse_instrument(id, &line, inst_map) {
            Ok(i) => out.push(i),
            Err(e) => errors.push(e),
        }
    }

    (out, errors)
}

pub(crate) fn build_instrument_map(
    instruments: &Vec<MmlInstrument>,
) -> Result<HashMap<IdentifierStr, usize>, Vec<ErrorWithPos<MmlLineError>>> {
    let mut out = HashMap::with_capacity(instruments.len());
    let mut errors = Vec::new();

    for (i, inst) in instruments.iter().enumerate() {
        if out.insert(inst.identifier.as_ref(), i).is_some() {
            errors.push(ErrorWithPos(
                inst.file_range.clone(),
                MmlLineError::DuplicateInstrumentName(inst.identifier.as_str().to_owned()),
            ));
        }
    }

    if errors.is_empty() {
        Ok(out)
    } else {
        Err(errors)
    }
}
