//! SPC700 assembler

// SPDX-FileCopyrightText: © 2026 Marcus Rowe <undisbeliever@gmail.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    errors::{FileErrors, LineNo},
    evaluator::{
        evaluate, evaluate_constexpr_address, evaluate_constexpr_u16, evaluate_u16v, evaluate_u8v,
        ExpressionError, ExpressionResult, ValueError,
    },
    file_loader::{split_file_lines, split_str_lines, AsmFileWithIncludes, SplitLines},
    file_parser::{
        parse_file, AsmLine, CodeBankStatement, FunctionTableDef, GlobalAsm, Procedure,
        StructSection, Var, VarBankStatement, VarsSection,
    },
    instructions::process_instruction,
    state::{
        is_symbol_name_valid, process_asserts, process_pending_output_expressions, State,
        SymbolError,
    },
    string::comma_iter,
};

use std::{
    collections::{hash_map::Entry, HashMap},
    ops::Range,
};

#[derive(Debug, PartialEq)]
pub enum AssemblerError<'s> {
    CodeBankNotSet,
    MultipleCodeBankStatements,
    InvalidCodeBankRange(u16, u16),
    CodeTooLarge(Range<u16>, i64),

    DuplicateVarBankName(&'s str),
    InvalidVarBankRange(u16, u16),

    ConstantError(&'s str, ExpressionError),
    ConstantCannotBeBoolean(&'s str),
    ConstantHasUnknownValue(&'s str),

    DbError(usize, ValueError<'s>),
    DwError(usize, ValueError<'s>),
    FunctionTableError(ValueError<'s>),

    InvalidStructName(&'s str),
    DuplicateStruct(&'s str),

    InvalidFieldName(&'s str),
    DuplicateField(&'s str),

    EmptyStruct,

    InvalidFtName(&'s str),
    DuplicateFtdef(&'s str),
    InvalidFtFunction(&'s str),
    FtdefNotFound(&'s str),

    CannotFindVarBank(&'s str),
    UnknownType(&'s str),
    InvalidArraySyntax,
    CannotNestArrays,
    ArrayTooLarge,

    CannotOpenProc(SymbolError<'s>),
    CannotOpenInline(SymbolError<'s>),
    EmptyProc,

    DuplicateInline(&'s str),
    UnusedInline(&'s str),
    EmptyInline,
    CannotUseArgumentsInInlineCall,
    CanOnlyUseInlineOnce,

    VarBankOverflows,
}

impl std::fmt::Display for AssemblerError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssemblerError::CodeBankNotSet => {
                write!(f, ".codebank must be set before writing code or data")
            }
            AssemblerError::MultipleCodeBankStatements => {
                write!(f, "multiple .codebank statements")
            }
            AssemblerError::InvalidCodeBankRange(from, to) => {
                write!(f, "invalid .codebank range: {from} - {to}")
            }
            AssemblerError::CodeTooLarge(code_bank, code_size) => write!(
                f,
                "code is too large: {} bytes, max: {}",
                code_size,
                code_bank.len()
            ),
            AssemblerError::DuplicateVarBankName(name) => {
                write!(f, "duplicate .varbank name: {name}")
            }
            AssemblerError::InvalidVarBankRange(from, to) => {
                write!(f, "invalid var bank range: {from}..{to}")
            }
            AssemblerError::ConstantError(expr, e) => write!(f, "constant error: {expr}: {e}"),
            AssemblerError::ConstantCannotBeBoolean(name) => {
                write!(f, "constant cannot be boolean: {name}")
            }
            AssemblerError::ConstantHasUnknownValue(name) => {
                write!(f, "constant has unknown value: {name}")
            }
            AssemblerError::DbError(i, e) => write!(f, ".db {i}: {e}"),
            AssemblerError::DwError(i, e) => write!(f, ".dw {i}: {e}"),
            AssemblerError::FunctionTableError(e) => write!(f, "{e}"),
            AssemblerError::InvalidStructName(name) => write!(f, "invalid struct name: {name}"),
            AssemblerError::DuplicateStruct(name) => write!(f, "duplicate .struct: {name}"),
            AssemblerError::InvalidFieldName(name) => write!(f, "invalid field name: {name}"),
            AssemblerError::DuplicateField(name) => write!(f, "duplicate struct field: {name}"),
            AssemblerError::EmptyStruct => write!(f, "empty struct"),
            AssemblerError::InvalidFtName(name) => write!(f, "invalid .ftdef name: {name}"),
            AssemblerError::DuplicateFtdef(name) => write!(f, "duplicate .ftdef: {name}"),
            AssemblerError::InvalidFtFunction(name) => {
                write!(f, "invalid function table function: {name}")
            }
            AssemblerError::FtdefNotFound(name) => write!(f, "cannot find .ftdef {name}"),
            AssemblerError::CannotFindVarBank(name) => write!(f, "cannot find .varbank {name}"),
            AssemblerError::UnknownType(name) => write!(f, "unknown type: {name}"),
            AssemblerError::InvalidArraySyntax => write!(f, "invalid array syntax"),
            AssemblerError::CannotNestArrays => write!(f, "cannot nest arrays"),
            AssemblerError::ArrayTooLarge => write!(f, "array too large"),
            AssemblerError::CannotOpenProc(e) => write!(f, "cannot open .proc: {e}"),
            AssemblerError::CannotOpenInline(e) => write!(f, "cannot open .inline: {e}"),
            AssemblerError::EmptyProc => write!(f, "empty .proc"),
            AssemblerError::DuplicateInline(name) => write!(f, "duplicate .inline: {name}"),
            AssemblerError::UnusedInline(name) => write!(f, ".inline {name} is unused"),
            AssemblerError::EmptyInline => write!(f, "empty .inline"),
            AssemblerError::CannotUseArgumentsInInlineCall => {
                write!(f, "cannot use arguments in an inline call")
            }
            AssemblerError::CanOnlyUseInlineOnce => write!(f, "can only use an .inline once"),
            AssemblerError::VarBankOverflows => write!(f, "var bank overflows"),
        }
    }
}

pub struct CompiledAsm {
    pub var_banks: Vec<VariableBank>,
    pub symbols: HashMap<String, Option<i64>>,
    pub output: Vec<u8>,
}

impl CompiledAsm {
    /// Gets the symbol's value
    ///
    /// Panics if `name` is not a symbol
    pub fn sym(&self, name: &str) -> i64 {
        match self.symbols.get(name) {
            Some(Some(v)) => *v,
            _ => panic!("Symbol {name:?} not found"),
        }
    }

    /// Gets the symbol's value without panicing
    pub fn get(&self, name: &str) -> Option<i64> {
        self.symbols.get(name).copied().flatten()
    }
}

pub struct VariableBank {
    pub name: String,
    pub range: Range<u16>,
    pub pos: u16,
}

#[derive(Debug)]
struct ChildType {
    name: String,
    offset: u16,
}

struct Type {
    size: u16,
    children: Vec<ChildType>,
}

fn default_types<'s>() -> HashMap<&'s str, Type> {
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
                        name: "l".to_owned(),
                        offset: 0,
                    },
                    ChildType {
                        name: "h".to_owned(),
                        offset: 1,
                    },
                ],
            },
        );
    }

    out
}

fn process_constant<'s>(
    line_no: LineNo,
    name: &'s str,
    expr: &'s str,
    state: &mut State<'s>,
    errors: &mut FileErrors<'s>,
) {
    match evaluate(expr, state) {
        ExpressionResult::Value(value) => match state.add_symbol(name, value) {
            Ok(()) => (),
            Err(e) => errors.push(line_no, e),
        },
        ExpressionResult::Boolean(_) => {
            errors.push(line_no, AssemblerError::ConstantCannotBeBoolean(expr));
        }
        ExpressionResult::Error(e) => {
            errors.push(line_no, AssemblerError::ConstantError(expr, e));
        }
        ExpressionResult::Unknown => {
            errors.push(line_no, AssemblerError::ConstantHasUnknownValue(expr));
        }
    }
}

fn process_code_bank<'s>(
    cb: CodeBankStatement<'s>,
    state: &State,
    errors: &mut FileErrors<'s>,
) -> Range<u16> {
    const ERROR_CODE_BANK: Range<u16> = 0x0200..0xffff;

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

fn process_var_bank<'s>(
    b: VarBankStatement<'s>,
    state: &State,
    var_banks: &mut Vec<VariableBank>,
    errors: &mut FileErrors<'s>,
) {
    if var_banks.iter().any(|o| o.name == b.name) {
        errors.push(b.line_no, AssemblerError::DuplicateVarBankName(b.name));
    }

    match (
        evaluate_constexpr_address(b.start_expr, state),
        evaluate_constexpr_address(b.end_expr, state),
    ) {
        (Ok(start), Ok(end)) => {
            if end > start {
                var_banks.push(VariableBank {
                    name: b.name.to_owned(),
                    range: start..end,
                    pos: start,
                })
            } else {
                errors.push(b.line_no, AssemblerError::InvalidVarBankRange(start, end));
            }
        }
        (e1, e2) => {
            if let Err(e) = e1 {
                errors.push(b.line_no, e);
            }
            if let Err(e) = e2 {
                errors.push(b.line_no, e);
            }
        }
    }
}

fn add_var_symbols<'s>(
    var: &Var<'s>,
    addr: u16,
    var_type: &Type,
    state: &mut State<'s>,
    errors: &mut FileErrors<'s>,
) {
    let addr = i64::from(addr);

    match state.add_symbol(var.name, addr) {
        Ok(()) => {
            for c in &var_type.children {
                if let Err(e) =
                    state.add_unchecked_scoped_symbol(var.name, &c.name, addr + i64::from(c.offset))
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
    types: &'a HashMap<&'s str, Type>,
    state: &State<'s>,
    errors: &mut FileErrors<'s>,
) -> Option<(&'a Type, u16)> {
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

fn process_struct<'s>(
    s: StructSection<'s>,
    state: &State<'s>,
    types: &mut HashMap<&'s str, Type>,
    errors: &mut FileErrors<'s>,
) {
    if !is_symbol_name_valid(s.name) {
        errors.push(s.line_no, AssemblerError::InvalidStructName(s.name));
    } else if types.get(s.name).is_some() {
        errors.push(s.line_no, AssemblerError::DuplicateStruct(s.name));
    }

    let mut offset = 0u16;
    let mut fields = Vec::new();

    for f in s.fields {
        if !is_symbol_name_valid(f.name) {
            errors.push(f.line_no, AssemblerError::InvalidFieldName(f.name));
        }

        if let Some((f_type, f_size)) = read_var_type(f.line_no, f.var_type, types, state, errors) {
            if fields.iter().any(|c: &ChildType| c.name == f.name) {
                errors.push(f.line_no, AssemblerError::DuplicateField(f.name));
            }

            fields.push(ChildType {
                name: f.name.to_owned(),
                offset,
            });

            for child in &f_type.children {
                fields.push(ChildType {
                    name: [f.name, ".", &child.name].concat(),
                    offset: offset + child.offset,
                });
            }

            offset = offset.saturating_add(f_size);
        }
    }

    if fields.is_empty() {
        errors.push(s.line_no, AssemblerError::EmptyStruct)
    }

    types.insert(
        s.name,
        Type {
            size: offset,
            children: fields,
        },
    );
}

fn process_variable<'s>(
    input: &Var<'s>,
    types: &HashMap<&'s str, Type>,
    bank: &mut VariableBank,
    state: &mut State<'s>,
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
    types: &HashMap<&'s str, Type>,
    banks: &mut [VariableBank],
    state: &mut State<'s>,
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

fn process_function_table_def<'s>(
    ft: FunctionTableDef<'s>,
    ft_map: &mut HashMap<&'s str, FunctionTableDef<'s>>,
    errors: &mut FileErrors<'s>,
) {
    let mut valid = true;

    if is_symbol_name_valid(ft.name) {
        if ft_map.contains_key(ft.name) {
            errors.push(ft.line_no, AssemblerError::DuplicateFtdef(ft.name));
            valid = false;
        }
    } else {
        errors.push(ft.line_no, AssemblerError::InvalidFtName(ft.name));
        valid = false;
    }

    for (f_line, f_name) in &ft.functions {
        if !is_symbol_name_valid(f_name) {
            errors.push(*f_line, AssemblerError::InvalidFtFunction(f_name));
            valid = false;
        }
    }

    if valid {
        ft_map.insert(ft.name, ft);
    }
}

fn process_function_table<'s>(
    line_no: LineNo,
    name: &'s str,
    ft_defs: &HashMap<&'s str, FunctionTableDef<'s>>,
    state: &mut State<'s>,
    errors: &mut FileErrors<'s>,
) {
    state.set_line_no(line_no);

    match ft_defs.get(name) {
        Some(ft) => {
            for (_, f) in &ft.functions {
                match evaluate_u16v(f, state) {
                    Ok(v) => state.write_u16v(v),
                    Err(e) => errors.push(line_no, AssemblerError::FunctionTableError(e)),
                }
            }
        }
        None => {
            errors.push(line_no, AssemblerError::FtdefNotFound(name));
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
    state: &mut State<'s>,
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

fn process_db<'s>(
    line_no: LineNo,
    arguments: &'s str,
    state: &mut State<'s>,
    errors: &mut FileErrors<'s>,
) {
    for (i, a) in comma_iter(arguments).enumerate() {
        match evaluate_u8v(a, state) {
            Ok(v) => state.write_u8v(v),
            Err(e) => errors.push(line_no, AssemblerError::DbError(i, e)),
        }
    }
}

fn process_dw<'s>(
    line_no: LineNo,
    arguments: &'s str,
    state: &mut State<'s>,
    errors: &mut FileErrors<'s>,
) {
    for (i, a) in comma_iter(arguments).enumerate() {
        match evaluate_u16v(a, state) {
            Ok(v) => state.write_u16v(v),
            Err(e) => errors.push(line_no, AssemblerError::DwError(i, e)),
        }
    }
}

fn process_asm_line<'s>(
    line_no: LineNo,
    line: AsmLine<'s>,
    inline_procs: &mut InlineProcs<'s>,
    state: &mut State<'s>,
    errors: &mut FileErrors<'s>,
) {
    state.set_line_no(line_no);

    match line {
        AsmLine::Label(label) => match state.add_symbol(label, state.program_counter()) {
            Ok(()) => (),
            Err(e) => errors.push(line_no, e),
        },
        AsmLine::Constant { name, expr } => process_constant(line_no, name, expr, state, errors),
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
        AsmLine::Db(arguments) => process_db(line_no, arguments, state, errors),
        AsmLine::Dw(arguments) => process_dw(line_no, arguments, state, errors),
        AsmLine::Assert(expr) => state.add_assert(line_no, expr),
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
    state: &mut State<'s>,
    errors: &mut FileErrors<'s>,
) {
    let child_symbols: Vec<&'s str> = proc
        .lines
        .iter()
        .filter_map(|(_, l)| match l {
            AsmLine::Constant { name, .. } => Some(*name),
            AsmLine::Label(l) => Some(*l),
            _ => None,
        })
        .collect();

    let proc_addr = state.program_counter();

    match state.add_symbol(proc.name, proc_addr) {
        Ok(()) => (),
        Err(e) => match code_type {
            ProcType::Procedure => errors.push(proc.line_no, AssemblerError::CannotOpenProc(e)),
            ProcType::Inline => errors.push(proc.line_no, AssemblerError::CannotOpenInline(e)),
        },
    }

    state.open_scope(proc.name, child_symbols);

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
    state: &mut State<'s>,
    errors: &mut FileErrors<'s>,
) {
    process_proc_or_inline(proc, ProcType::Procedure, inline_procs, state, errors);
}

fn check_code_size(code_bank: Option<Range<u16>>, state: &State, errors: &mut FileErrors) {
    if let Some(code_bank) = code_bank {
        let i64_code_bank = i64::from(code_bank.start)..i64::from(code_bank.end);
        if !i64_code_bank.contains(&state.program_counter()) {
            errors.push(
                LineNo(0, 0),
                AssemblerError::CodeTooLarge(code_bank, state.program_counter()),
            );
        }
    }
}

struct CodeBankSetTest(bool);

impl CodeBankSetTest {
    fn new() -> Self {
        Self(false)
    }

    fn set(&mut self) {
        self.0 = true;
    }

    fn check_codebank_set(&mut self, line_no: LineNo, errors: &mut FileErrors) {
        if !self.0 {
            errors.push(line_no, AssemblerError::CodeBankNotSet);
            self.0 = true;
        }
    }
}

fn assemble_lines<'s>(lines: SplitLines<'s>) -> Result<CompiledAsm, FileErrors<'s>> {
    let mut errors = FileErrors::new();

    let file = parse_file(lines, &mut errors);

    let mut inline_procs = prepare_inlines(file.inlines, &mut errors);

    let mut state = State::new(0);
    let mut code_bank = None;
    let mut cbst = CodeBankSetTest::new();
    let mut var_banks = Vec::new();
    let mut types = default_types();
    let mut ft_defs = HashMap::new();

    for a in file.assembly {
        match a {
            GlobalAsm::CodeBank(cb) => {
                if code_bank.is_none() {
                    let cb = process_code_bank(cb, &state, &mut errors);
                    state.set_pc_base(cb.start);
                    code_bank = Some(cb);
                    cbst.set();
                } else {
                    errors.push(cb.line_no, AssemblerError::MultipleCodeBankStatements);
                }
            }
            GlobalAsm::VarBank(v) => {
                process_var_bank(v, &state, &mut var_banks, &mut errors);
            }
            GlobalAsm::Struct(s) => process_struct(s, &state, &mut types, &mut errors),
            GlobalAsm::Vars(v) => {
                process_var_section(v, &types, &mut var_banks, &mut state, &mut errors)
            }
            GlobalAsm::FunctionTableDef(f) => {
                process_function_table_def(f, &mut ft_defs, &mut errors)
            }

            GlobalAsm::FunctionTable(line_no, name) => {
                cbst.check_codebank_set(line_no, &mut errors);
                process_function_table(line_no, name, &ft_defs, &mut state, &mut errors)
            }
            GlobalAsm::Procedure(proc) => {
                cbst.check_codebank_set(proc.line_no, &mut errors);
                process_proc(proc, &mut inline_procs, &mut state, &mut errors)
            }
            GlobalAsm::AsmLine(line_no, line) => {
                if !matches!(line, AsmLine::Constant { .. }) {
                    cbst.check_codebank_set(line_no, &mut errors);
                }
                process_asm_line(line_no, line, &mut inline_procs, &mut state, &mut errors)
            }
        }
    }

    process_pending_output_expressions(&mut state, &mut errors);
    generate_unused_inline_errors(inline_procs, &mut errors);
    process_asserts(&mut state, &mut errors);
    check_code_size(code_bank.clone(), &state, &mut errors);

    if errors.is_empty() {
        let (output, symbols) = state.take_output_and_symbols();

        // Make sure there are no variable overruns
        for b in &var_banks {
            assert!(b.pos >= b.range.start && b.pos <= b.range.end);
        }

        // Make sure there is a `.codebank` statement if there is any output
        assert!(code_bank.is_some() || output.is_empty());

        Ok(CompiledAsm {
            var_banks,
            output,
            symbols,
        })
    } else {
        Err(errors)
    }
}

pub fn assemble<'s>(input: &'s str) -> Result<CompiledAsm, FileErrors<'s>> {
    assemble_lines(split_str_lines(input))
}

pub fn assemble_loaded_file<'s>(
    input: &'s AsmFileWithIncludes,
) -> Result<CompiledAsm, FileErrors<'s>> {
    assemble_lines(split_file_lines(input))
}
