//! SPC700 assembler

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    errors::{FileErrors, LineNo},
    evaluator::{
        evaluate, evaluate_constexpr_address, evaluate_constexpr_u16, ExpressionError,
        ExpressionResult,
    },
    file_parser::{
        parse_file, AsmLine, AsmOrProc, CodeBankStatement, Constant, Procedure, Var,
        VarBankStatement, VarsSection,
    },
    instructions::process_instruction,
    state::{process_pending_output_expressions, State, SymbolError},
};

use std::{
    collections::{hash_map::Entry, HashMap},
    ops::Range,
};

#[derive(Debug, PartialEq)]
pub enum AssemblerError<'s> {
    NoCodeBankStatement,
    InvalidCodeBankRange(u16, u16),
    CodeTooLarge(Range<u16>, i64),

    DuplicateVarBankName(&'s str),
    InvalidVarBankRange(u16, u16),

    ConstantError(&'s str, ExpressionError),
    ConstantCannotBeBoolean(&'s str),
    ConstantWithUnknownValue(&'s str),

    CannotFindVarBank(&'s str),
    UnknownType(&'s str),
    InvalidArraySyntax,
    CannotNestArrays,
    ArrayTooLarge,

    CannotOpenProc(&'s str, SymbolError),
    EmptyProc,

    DuplicateInline(&'s str),
    UnusedInline(&'s str),
    EmptyInline,
    CannotUseArgumentsInInlineCall,
    CanOnlyUseInlineOnce,

    VarBankOverflows,
}

pub struct CompiledAsm {
    pub banks: Vec<VariableBank>,
    pub symbols: HashMap<String, i64>,
    pub output: Vec<u8>,
}

pub struct VariableBank {
    pub name: String,
    pub range: Range<u16>,
    pub pos: u16,
}

struct ChildType<'s> {
    name: &'s str,
    offset: u16,
}

struct Type<'s> {
    size: u16,
    children: Vec<ChildType<'s>>,
}

fn default_types<'s>() -> HashMap<&'s str, Type<'s>> {
    let mut out = HashMap::new();

    out.insert(
        "u8",
        Type {
            size: 1,
            children: Vec::new(),
        },
    );
    out.insert(
        "s8",
        Type {
            size: 1,
            children: Vec::new(),
        },
    );

    for name in ["u16", "s16", "ptr"] {
        out.insert(
            name,
            Type {
                size: 2,
                children: vec![
                    ChildType {
                        name: "l",
                        offset: 0,
                    },
                    ChildType {
                        name: "h",
                        offset: 1,
                    },
                ],
            },
        );
    }

    out
}

fn process_constants<'s>(
    constants: Vec<Constant<'s>>,
    state: &mut State,
    errors: &mut FileErrors<'s>,
) -> Vec<Constant<'s>> {
    let mut constants = constants;

    constants.retain(|c| match evaluate(c.expr, state) {
        ExpressionResult::Value(value) => {
            match state.add_symbol(c.name, value) {
                Ok(()) => (),
                Err(e) => {
                    errors.push(c.line_no, e);
                }
            }
            false
        }
        ExpressionResult::Boolean(_) => {
            errors.push(c.line_no, AssemblerError::ConstantCannotBeBoolean(c.expr));
            false
        }
        ExpressionResult::Error(e) => {
            errors.push(c.line_no, AssemblerError::ConstantError(c.expr, e));
            false
        }
        ExpressionResult::Unknown => true,
    });

    constants
}

fn process_code_bank<'s>(
    cb: Option<CodeBankStatement<'s>>,
    state: &State,
    errors: &mut FileErrors<'s>,
) -> Range<u16> {
    const ERROR_CODE_BANK: Range<u16> = 0x0200..0xffff;

    match cb {
        Some(cb) => {
            match (
                evaluate_constexpr_address(cb.start_expr, state),
                evaluate_constexpr_address(cb.end_expr, state),
            ) {
                (Ok(start), Ok(end)) if start < end => start..end,
                (Ok(start), Ok(end)) => {
                    errors.push(cb.line_no, AssemblerError::InvalidVarBankRange(start, end));
                    ERROR_CODE_BANK
                }
                (e1, e2) => {
                    if let Err(e) = e1 {
                        errors.push(cb.line_no, e);
                    }
                    if let Err(e) = e2 {
                        errors.push(cb.line_no, e);
                    }
                    ERROR_CODE_BANK
                }
            }
        }
        None => {
            errors.push(LineNo(0), AssemblerError::NoCodeBankStatement);
            ERROR_CODE_BANK
        }
    }
}

fn process_var_bank<'s>(
    b: &VarBankStatement<'s>,
    state: &State,
    errors: &mut FileErrors<'s>,
) -> Option<VariableBank> {
    let mut add_error = |e| errors.push(b.line_no, e);

    match (
        evaluate_constexpr_address(b.start_expr, state),
        evaluate_constexpr_address(b.end_expr, state),
    ) {
        (Ok(start), Ok(end)) => {
            if end > start {
                Some(VariableBank {
                    name: b.name.to_owned(),
                    range: start..end,
                    pos: start,
                })
            } else {
                errors.push(b.line_no, AssemblerError::InvalidVarBankRange(start, end));
                None
            }
        }
        (e1, e2) => {
            if let Err(e) = e1 {
                add_error(e)
            }
            if let Err(e) = e2 {
                add_error(e)
            }
            None
        }
    }
}

fn process_var_banks<'s>(
    input: Vec<VarBankStatement<'s>>,
    state: &State,
    errors: &mut FileErrors<'s>,
) -> Vec<VariableBank> {
    let mut out = Vec::<VariableBank>::with_capacity(input.len());

    for b in &input {
        if out.iter().any(|o| o.name == b.name) {
            errors.push(b.line_no, AssemblerError::DuplicateVarBankName(b.name))
        }

        if let Some(b) = process_var_bank(b, state, errors) {
            out.push(b)
        }
    }

    out
}

fn add_var_symbols(
    var: &Var,
    addr: u16,
    var_type: &Type,
    state: &mut State,
    errors: &mut FileErrors,
) {
    let addr = i64::from(addr);

    match state.add_symbol(var.name, addr) {
        Ok(()) => {
            for c in &var_type.children {
                if let Err(e) =
                    state.add_scoped_symbol(var.name, c.name, addr + i64::from(c.offset))
                {
                    errors.push(var.line_no, e);
                }
            }
        }
        Err(e) => {
            errors.push(var.line_no, e);
        }
    }
}

fn read_var_type<'s, 'a>(
    line_no: LineNo,
    var_type: &'s str,
    types: &'a HashMap<&'s str, Type<'s>>,
    state: &State,
    errors: &mut FileErrors<'s>,
) -> Option<(&'a Type<'s>, u16)> {
    if !var_type.starts_with("[") {
        match types.get(var_type) {
            Some(var_type) => Some((var_type, var_type.size)),
            None => {
                errors.push(line_no, AssemblerError::UnknownType(var_type));
                None
            }
        }
    } else {
        if var_type.bytes().filter(|&c| c == b'[').count() > 1 {
            errors.push(line_no, AssemblerError::CannotNestArrays);
            return None;
        }

        match var_type
            .strip_prefix("[")
            .and_then(|s| s.strip_suffix("]"))
            .and_then(|s| s.split_once(":"))
        {
            Some((var_type, count_expr)) => {
                let var_type = var_type.trim();
                let count_expr = count_expr.trim();

                match (
                    types.get(var_type),
                    evaluate_constexpr_u16(count_expr, state),
                ) {
                    (Some(var_type), Ok(count)) => match count.checked_mul(var_type.size) {
                        Some(size) => Some((var_type, size)),
                        None => {
                            errors.push(line_no, AssemblerError::ArrayTooLarge);
                            None
                        }
                    },
                    (vt, ce) => {
                        if vt.is_none() {
                            errors.push(line_no, AssemblerError::UnknownType(var_type));
                        }
                        if let Err(ce) = ce {
                            errors.push(line_no, ce);
                        }
                        None
                    }
                }
            }
            None => {
                errors.push(line_no, AssemblerError::InvalidArraySyntax);
                None
            }
        }
    }
}

fn process_variable<'s>(
    input: &Var<'s>,
    types: &HashMap<&'s str, Type<'s>>,
    bank: &mut VariableBank,
    state: &mut State,
    errors: &mut FileErrors<'s>,
) {
    if let Some((var_type, size)) =
        read_var_type(input.line_no, input.var_type, types, state, errors)
    {
        add_var_symbols(input, bank.pos, var_type, state, errors);

        let new_pos = bank.pos.saturating_add(size);
        if new_pos > bank.range.end && bank.pos <= bank.range.end {
            errors.push(input.line_no, AssemblerError::VarBankOverflows);
        }
        bank.pos = new_pos;
    }
}

fn process_var_section<'s>(
    input: VarsSection<'s>,
    types: &HashMap<&'s str, Type<'s>>,
    banks: &mut [VariableBank],
    state: &mut State,
    errors: &mut FileErrors<'s>,
) {
    match banks.iter_mut().find(|b| b.name == input.bank) {
        Some(b) => {
            for v in &input.variables {
                process_variable(v, types, b, state, errors);
            }
        }
        None => {
            errors.push(input.line_no, AssemblerError::CannotFindVarBank(input.bank));

            let mut dummy = VariableBank {
                name: String::new(),
                range: 0..0xff,
                pos: 0,
            };
            for v in &input.variables {
                process_variable(v, types, &mut dummy, state, errors);
            }
        }
    }
}

enum InlineProc<'s> {
    Inline(Procedure<'s>),
    Taken,
}

struct InlineProcs<'s> {
    map: HashMap<&'s str, usize>,
    inlines: Vec<InlineProc<'s>>,
}

impl<'s> InlineProcs<'s> {
    fn find_and_take(&mut self, name: &str) -> Option<InlineProc<'s>> {
        match self.map.get(name) {
            Some(&i) => Some(std::mem::replace(&mut self.inlines[i], InlineProc::Taken)),
            None => None,
        }
    }
}

fn prepare_inlines<'s>(input: Vec<Procedure<'s>>, errors: &mut FileErrors<'s>) -> InlineProcs<'s> {
    let mut map = HashMap::new();
    let mut inlines = Vec::with_capacity(input.len());

    for i in input {
        match map.entry(i.name) {
            Entry::Vacant(v) => {
                v.insert(inlines.len());
                inlines.push(InlineProc::Inline(i));
            }
            Entry::Occupied(m) => {
                errors.push(i.line_no, AssemblerError::DuplicateInline(m.key()));
            }
        }
    }

    InlineProcs { map, inlines }
}

fn generate_unused_inline_errors<'s>(inlines: InlineProcs<'s>, errors: &mut FileErrors<'s>) {
    for i in inlines.inlines {
        match i {
            InlineProc::Taken => (),
            InlineProc::Inline(unused) => {
                errors.push(unused.line_no, AssemblerError::UnusedInline(unused.name));
            }
        }
    }
}

fn process_inline<'s>(
    caller_line_no: LineNo,
    inline: InlineProc<'s>,
    inline_procs: &mut InlineProcs<'s>,
    state: &mut State,
    errors: &mut FileErrors<'s>,
) {
    let caller_scope = state.take_scope();

    match inline {
        InlineProc::Inline(code) => {
            process_proc_or_inline(code, ProcType::Inline, inline_procs, state, errors);
        }
        InlineProc::Taken => {
            errors.push(caller_line_no, AssemblerError::CanOnlyUseInlineOnce);
        }
    }

    state.restore_scope(caller_scope);
}

fn process_asm_line<'s>(
    line_no: LineNo,
    line: AsmLine<'s>,
    inline_procs: &mut InlineProcs<'s>,
    state: &mut State,
    errors: &mut FileErrors<'s>,
) {
    state.set_line_no(line_no);

    match line {
        AsmLine::Label(label) => match state.add_symbol(label, state.program_counter()) {
            Ok(()) => (),
            Err(e) => errors.push(line_no, e),
        },
        AsmLine::Instruction(instruction, arguments) => {
            match inline_procs.find_and_take(instruction) {
                None => match process_instruction(instruction, arguments, state) {
                    Ok(()) => (),
                    Err(e) => errors.push(line_no, e),
                },
                Some(inline) => {
                    if !arguments.is_empty() {
                        errors.push(line_no, AssemblerError::CannotUseArgumentsInInlineCall)
                    }
                    process_inline(line_no, inline, inline_procs, state, errors);
                }
            }
        }
    }
}

enum ProcType {
    Procedure,
    Inline,
}

fn process_proc_or_inline<'s>(
    proc: Procedure<'s>,
    code_type: ProcType,
    inline_procs: &mut InlineProcs<'s>,
    state: &mut State,
    errors: &mut FileErrors<'s>,
) {
    let labels: Vec<&'s str> = proc
        .lines
        .iter()
        .filter_map(|(_, l)| match l {
            AsmLine::Label(l) => Some(*l),
            _ => None,
        })
        .collect();

    let proc_addr = state.program_counter();

    match state.add_symbol(proc.name, proc_addr) {
        Ok(()) => (),
        Err(e) => errors.push(proc.line_no, AssemblerError::CannotOpenProc(proc.name, e)),
    }

    state.open_scope(proc.name, labels);

    let unknown = process_constants(proc.constants, state, errors);
    for c in unknown {
        errors.push(c.line_no, AssemblerError::ConstantWithUnknownValue(c.expr))
    }

    for (line_no, asm) in proc.lines {
        process_asm_line(line_no, asm, inline_procs, state, errors);
    }

    if state.program_counter() == proc_addr {
        errors.push(
            proc.end_line_no,
            match code_type {
                ProcType::Procedure => AssemblerError::EmptyProc,
                ProcType::Inline => AssemblerError::EmptyInline,
            },
        );
    }

    state.close_scope();
}

fn process_proc<'s>(
    proc: Procedure<'s>,
    inline_procs: &mut InlineProcs<'s>,
    state: &mut State,
    errors: &mut FileErrors<'s>,
) {
    process_proc_or_inline(proc, ProcType::Procedure, inline_procs, state, errors);
}

fn process_assembly<'s>(
    assembly: Vec<AsmOrProc<'s>>,
    inline_procs: &mut InlineProcs<'s>,
    state: &mut State,
    errors: &mut FileErrors<'s>,
) {
    for a in assembly {
        match a {
            AsmOrProc::AsmLine(line_no, line) => {
                process_asm_line(line_no, line, inline_procs, state, errors)
            }
            AsmOrProc::Procedure(proc) => process_proc(proc, inline_procs, state, errors),
        }
    }
}

pub fn assemble<'s>(input: &'s str) -> Result<CompiledAsm, FileErrors<'s>> {
    let mut errors = FileErrors::new();

    let types = default_types();

    let file = parse_file(input, &mut errors);

    let mut inline_procs = prepare_inlines(file.inlines, &mut errors);

    let mut state = State::new(0);

    let pending_constants = process_constants(file.global_constants, &mut state, &mut errors);

    let code_bank = process_code_bank(file.code_bank, &state, &mut errors);
    let mut banks = process_var_banks(file.var_banks, &state, &mut errors);

    for v in file.vars {
        process_var_section(v, &types, &mut banks, &mut state, &mut errors);
    }

    let pending_constants = process_constants(pending_constants, &mut state, &mut errors);

    state.set_pc_base(code_bank.start);

    process_assembly(file.assembly, &mut inline_procs, &mut state, &mut errors);

    let pending_constants = process_constants(pending_constants, &mut state, &mut errors);

    for c in pending_constants {
        errors.push(c.line_no, AssemblerError::ConstantWithUnknownValue(c.expr))
    }

    process_pending_output_expressions(&mut state, &mut errors);

    let i64_code_bank = i64::from(code_bank.start)..i64::from(code_bank.end);
    if !i64_code_bank.contains(&state.program_counter()) {
        errors.push(
            LineNo(0),
            AssemblerError::CodeTooLarge(code_bank, state.program_counter()),
        );
    }

    generate_unused_inline_errors(inline_procs, &mut errors);

    if !errors.is_empty() {
        return Err(errors);
    }

    // Make sure there are no variable overruns
    for b in &banks {
        assert!(b.pos >= b.range.start && b.pos <= b.range.end);
    }

    if errors.is_empty() {
        let (output, symbols) = state.take_output_and_symbols();

        // ::TODO remove this step::
        let symbols = symbols.into_iter().map(|(k, v)| (k, v.unwrap())).collect();

        Ok(CompiledAsm {
            banks,
            output,
            symbols,
        })
    } else {
        Err(errors)
    }
}
