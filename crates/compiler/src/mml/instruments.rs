//! MML instrument parser

// SPDX-FileCopyrightText: © 2023 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use super::IdentifierStr;

use crate::bytecode::InstrumentId;
use crate::command_compiler::commands::MmlInstrument;
use crate::data;
use crate::data::UniqueNamesList;
use crate::envelope::Envelope;
use crate::errors::{ErrorWithPos, MmlLineError};
use crate::file_pos::Line;

use std::collections::HashMap;

fn parse_instrument(
    id: IdentifierStr,
    line: &Line,
    inst_map: &UniqueNamesList<data::InstrumentOrSample>,
) -> Result<MmlInstrument, ErrorWithPos<MmlLineError>> {
    if line.text.is_empty() {
        return Err(ErrorWithPos(line.range(), MmlLineError::NoInstrument));
    }

    let (inst_name, args) = match line.split_once() {
        Some((n, a)) => {
            let a = a.trim_start();
            if !a.text.is_empty() {
                (n, Some(a))
            } else {
                (n, None)
            }
        }
        None => (line.text, None),
    };

    let inst_name_err = |e| Err(ErrorWithPos(line.position.to_range_str_len(inst_name), e));

    let instrument_id = match inst_map.get_name_index(inst_name) {
        Some(i) => i,
        None => {
            return inst_name_err(MmlLineError::CannotFindInstrument(inst_name.to_owned()));
        }
    };

    let instrument_id = match InstrumentId::try_from(instrument_id) {
        Ok(i) => i,
        Err(e) => return inst_name_err(e.into()),
    };

    let mut envelope = None;
    if let Some(args) = args {
        let arg = args.text;
        let arg_range = args.range();
        let arg_err = |e| Err(ErrorWithPos(arg_range, e));

        match Envelope::try_from_envelope_str(arg) {
            Ok(Envelope::Adsr(a)) => {
                envelope = Some(Envelope::Adsr(a));
            }
            Ok(Envelope::Gain(g)) => {
                envelope = Some(Envelope::Gain(g));
            }
            Err(e) => {
                return arg_err(MmlLineError::ValueError(e));
            }
        }
    }

    Ok(MmlInstrument {
        identifier: id.to_owned(),
        file_range: line.range(),
        instrument_id,
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
) -> Result<HashMap<IdentifierStr<'_>, &MmlInstrument>, Vec<ErrorWithPos<MmlLineError>>> {
    let mut out = HashMap::with_capacity(instruments.len());
    let mut errors = Vec::new();

    for inst in instruments.iter() {
        if out.insert(inst.identifier.as_ref(), inst).is_some() {
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
