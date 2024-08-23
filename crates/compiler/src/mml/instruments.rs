//! MML instrument parser

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::{IdentifierBuf, IdentifierStr};

use crate::bytecode::InstrumentId;
use crate::data::UniqueNamesList;
use crate::data::{self, InstrumentOrSample};
use crate::envelope::Envelope;
use crate::errors::{ErrorWithPos, MmlLineError};
use crate::file_pos::{FilePosRange, Line};
use crate::notes::Note;
use crate::samples::note_range;

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct MmlInstrument {
    pub(crate) identifier: IdentifierBuf,

    pub(crate) file_range: FilePosRange,

    pub(crate) instrument_id: InstrumentId,

    pub(crate) first_note: Note,
    pub(crate) last_note: Note,

    // No envelope override or envelope override matches instrument
    pub(crate) envelope_unchanged: bool,
    pub(crate) envelope: Envelope,
}

fn parse_instrument(
    id: IdentifierStr,
    line: &Line,
    inst_map: &UniqueNamesList<data::InstrumentOrSample>,
) -> Result<MmlInstrument, ErrorWithPos<MmlLineError>> {
    if line.text.is_empty() {
        return Err(ErrorWithPos(line.range(), MmlLineError::NoInstrument));
    }

    let (inst_name, args) = match line.split_once() {
        Some((n, a)) => (n, Some(a)),
        None => (line.text, None),
    };

    let inst_name_err = |e| Err(ErrorWithPos(line.position.to_range_str_len(inst_name), e));

    let (instrument_id, inst) = match inst_map.get_with_index(inst_name) {
        Some((inst_id, inst)) => (inst_id, inst),
        None => {
            return inst_name_err(MmlLineError::CannotFindInstrument(inst_name.to_owned()));
        }
    };

    let instrument_id = match InstrumentId::try_from(instrument_id) {
        Ok(i) => i,
        Err(e) => return inst_name_err(e.into()),
    };

    let source_envelope = match inst {
        InstrumentOrSample::Instrument(inst) => &inst.envelope,
        InstrumentOrSample::Sample(sample) => &sample.envelope,
    };
    let mut envelope = *source_envelope;

    if let Some(args) = args {
        let arg = args.text;
        let arg_range = args.range();
        let arg_err = |e| Err(ErrorWithPos(arg_range, e));

        match Envelope::try_from_envelope_str(arg) {
            Ok(Envelope::Adsr(a)) => {
                envelope = Envelope::Adsr(a);
            }
            Ok(Envelope::Gain(g)) => {
                envelope = Envelope::Gain(g);
            }
            Err(e) => {
                return arg_err(MmlLineError::ValueError(e));
            }
        }
    }

    let (first_note, last_note) = match note_range(inst) {
        Ok(o) => o,
        Err(e) => return Err(ErrorWithPos(line.range(), e.into())),
    };

    Ok(MmlInstrument {
        identifier: id.to_owned(),
        file_range: line.range(),
        instrument_id,
        first_note,
        last_note,
        envelope_unchanged: &envelope == source_envelope,
        envelope,
    })
}

pub fn parse_instruments(
    instrument_lines: Vec<(IdentifierStr, Line)>,
    inst_map: &UniqueNamesList<data::InstrumentOrSample>,
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
    instruments: &[MmlInstrument],
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
