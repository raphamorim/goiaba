// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! Go AST to LLVM IR lowering.
//!
//! Unlike the wasm backend this pass tracks the Go type of every expression,
//! because LLVM IR is typed. Unsupported constructs are reported as errors
//! instead of being skipped.
//!
//! Lowering model:
//! - every local lives in an `alloca` (LLVM's mem2reg turns them into SSA);
//! - a local whose address is taken is allocated on the heap instead;
//! - `int` is 64 bits, strings are `{ptr, len}`, slices are `{ptr, len, cap}`;
//! - structs and arrays are LLVM first-class aggregates.

use super::runtime::{c_escape, prelude};
use crate::parser::ast::{self, Decl, Expr, Spec, Stmt};
use crate::parser::objects::AstObjects;
use crate::parser::{Token, parse_str};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::Write as _;

type R<T> = Result<T, String>;

#[derive(Debug, Clone, PartialEq)]
enum Ty {
    Int { bits: u32, signed: bool },
    Float(u32),
    Bool,
    Str,
    Struct(String),
    Ptr(Box<Ty>),
    Array(Box<Ty>, u64),
    Slice(Box<Ty>),
    Tuple(Vec<Ty>),
    Void,
    UntypedInt,
    UntypedRune,
    UntypedFloat,
    Nil,
}

const INT: Ty = Ty::Int {
    bits: 64,
    signed: true,
};

impl Ty {
    /// The LLVM spelling of this type.
    fn ll(&self) -> String {
        match self {
            Ty::Int { bits, .. } => format!("i{bits}"),
            Ty::Float(32) => "float".into(),
            Ty::Float(_) | Ty::UntypedFloat => "double".into(),
            Ty::Bool => "i1".into(),
            Ty::Str => "%string".into(),
            Ty::Struct(name) => format!("%\"main.{name}\""),
            Ty::Ptr(_) | Ty::Nil => "ptr".into(),
            Ty::Array(elem, n) => format!("[{n} x {}]", elem.ll()),
            Ty::Slice(_) => "%slice".into(),
            Ty::Tuple(items) => {
                let parts: Vec<String> = items.iter().map(Ty::ll).collect();
                format!("{{ {} }}", parts.join(", "))
            }
            Ty::Void => "void".into(),
            Ty::UntypedInt | Ty::UntypedRune => "i64".into(),
        }
    }

    fn is_untyped_num(&self) -> bool {
        matches!(self, Ty::UntypedInt | Ty::UntypedRune | Ty::UntypedFloat)
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Int { bits: 64, signed } => f.write_str(if *signed { "int" } else { "uint" }),
            Ty::Int { bits, signed } => {
                write!(f, "{}int{bits}", if *signed { "" } else { "u" })
            }
            Ty::Float(bits) => write!(f, "float{bits}"),
            Ty::Bool => f.write_str("bool"),
            Ty::Str => f.write_str("string"),
            Ty::Struct(name) => f.write_str(name),
            Ty::Ptr(inner) => write!(f, "*{inner}"),
            Ty::Array(elem, n) => write!(f, "[{n}]{elem}"),
            Ty::Slice(elem) => write!(f, "[]{elem}"),
            Ty::Tuple(items) => {
                let parts: Vec<String> = items.iter().map(|t| t.to_string()).collect();
                write!(f, "({})", parts.join(", "))
            }
            Ty::Void => f.write_str("no value"),
            Ty::UntypedInt => f.write_str("untyped int"),
            Ty::UntypedRune => f.write_str("untyped rune"),
            Ty::UntypedFloat => f.write_str("untyped float"),
            Ty::Nil => f.write_str("untyped nil"),
        }
    }
}

/// A compile-time constant value.
#[derive(Debug, Clone, Copy)]
enum K {
    Int(i128),
    Float(f64),
}

impl K {
    fn as_f64(self) -> f64 {
        match self {
            K::Int(v) => v as f64,
            K::Float(v) => v,
        }
    }
}

/// A typed operand: an SSA name or a constant. `k` is set only when `repr`
/// is a numeric constant.
#[derive(Debug, Clone)]
struct Val {
    ty: Ty,
    repr: String,
    k: Option<K>,
}

impl Val {
    fn new(ty: Ty, repr: impl Into<String>) -> Self {
        Val {
            ty,
            repr: repr.into(),
            k: None,
        }
    }

    fn int(ty: Ty, v: i128) -> Self {
        Val {
            ty,
            repr: v.to_string(),
            k: Some(K::Int(v)),
        }
    }

    fn float(ty: Ty, v: f64) -> Self {
        let bits = if ty == Ty::Float(32) { 32 } else { 64 };
        Val {
            ty,
            repr: float_lit(v, bits),
            k: Some(K::Float(v)),
        }
    }

    fn bool(v: bool) -> Self {
        Val::new(Ty::Bool, if v { "true" } else { "false" })
    }

    fn void() -> Self {
        Val::new(Ty::Void, "")
    }
}

/// LLVM accepts any double as a 16-digit hex literal. A `float` constant must
/// be exactly representable, so it is rounded to f32 first.
fn float_lit(v: f64, bits: u32) -> String {
    let v = if bits == 32 { (v as f32) as f64 } else { v };
    format!("0x{:016X}", v.to_bits())
}

#[derive(Debug, Clone)]
enum Binding {
    Var { ptr: String, ty: Ty, heap: bool },
    Const(Val),
}

/// Where an expression lives: in memory (assignable) or only as a value.
enum Loc {
    Place(String, Ty),
    Value(Val),
}

#[derive(Debug, Clone)]
struct FuncSig {
    symbol: String,
    params: Vec<Ty>,
    ret: Ty,
    /// `Some(true)` for a pointer-receiver method, `Some(false)` for a value receiver.
    ptr_recv: Option<bool>,
}

struct Shape {
    key: String,
    params: Vec<(String, Ty)>,
    ret: Ty,
    ptr_recv: Option<bool>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    AndNot,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl Op {
    fn from_binary(tok: &Token) -> Option<Op> {
        Some(match tok {
            Token::ADD => Op::Add,
            Token::SUB => Op::Sub,
            Token::MUL => Op::Mul,
            Token::QUO => Op::Div,
            Token::REM => Op::Rem,
            Token::AND => Op::And,
            Token::OR => Op::Or,
            Token::XOR => Op::Xor,
            Token::AND_NOT => Op::AndNot,
            Token::SHL => Op::Shl,
            Token::SHR => Op::Shr,
            Token::EQL => Op::Eq,
            Token::NEQ => Op::Ne,
            Token::LSS => Op::Lt,
            Token::LEQ => Op::Le,
            Token::GTR => Op::Gt,
            Token::GEQ => Op::Ge,
            _ => return None,
        })
    }

    fn from_assign(tok: &Token) -> Option<Op> {
        Some(match tok {
            Token::ADD_ASSIGN => Op::Add,
            Token::SUB_ASSIGN => Op::Sub,
            Token::MUL_ASSIGN => Op::Mul,
            Token::QUO_ASSIGN => Op::Div,
            Token::REM_ASSIGN => Op::Rem,
            Token::AND_ASSIGN => Op::And,
            Token::OR_ASSIGN => Op::Or,
            Token::XOR_ASSIGN => Op::Xor,
            Token::AND_NOT_ASSIGN => Op::AndNot,
            Token::SHL_ASSIGN => Op::Shl,
            Token::SHR_ASSIGN => Op::Shr,
            _ => return None,
        })
    }

    fn is_comparison(self) -> bool {
        matches!(self, Op::Eq | Op::Ne | Op::Lt | Op::Le | Op::Gt | Op::Ge)
    }

    fn symbol(self) -> &'static str {
        match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Rem => "%",
            Op::And => "&",
            Op::Or => "|",
            Op::Xor => "^",
            Op::AndNot => "&^",
            Op::Shl => "<<",
            Op::Shr => ">>",
            Op::Eq => "==",
            Op::Ne => "!=",
            Op::Lt => "<",
            Op::Le => "<=",
            Op::Gt => ">",
            Op::Ge => ">=",
        }
    }
}

/// Per-function emission state.
struct FnCtx {
    allocas: Vec<String>,
    body: Vec<String>,
    counter: usize,
    /// True when the current block already ends in a terminator.
    terminated: bool,
    /// Label of the block being filled.
    cur: String,
    scopes: Vec<HashMap<String, Binding>>,
    ret: Ty,
    /// Innermost-last `(break target, continue target)`; a switch has no continue target.
    targets: Vec<(String, Option<String>)>,
    /// Locals that must live on the heap because their address is taken.
    escaping: HashSet<String>,
    /// Set when `escaping` grew, so the function has to be lowered again.
    retry: bool,
}

impl FnCtx {
    fn new(ret: Ty, escaping: HashSet<String>) -> Self {
        FnCtx {
            allocas: Vec::new(),
            body: Vec::new(),
            counter: 0,
            terminated: false,
            cur: "entry".into(),
            scopes: Vec::new(),
            ret,
            targets: Vec::new(),
            escaping,
            retry: false,
        }
    }
}

struct Compiler<'a> {
    objs: &'a AstObjects,
    structs: HashMap<String, Vec<(String, Ty)>>,
    struct_order: Vec<String>,
    aliases: HashMap<String, Ty>,
    funcs: HashMap<String, FuncSig>,
    globals: HashMap<String, Binding>,
    packages: HashSet<String>,
    strings: Vec<Vec<u8>>,
    string_ids: HashMap<Vec<u8>, usize>,
    global_defs: Vec<String>,
    fn_defs: Vec<String>,
    f: FnCtx,
}

/// Compiles Go source to a textual LLVM IR module.
pub fn compile_str(go_source: &str) -> Result<String, Box<dyn std::error::Error>> {
    let (objs, file) = parse_str(go_source)?;
    let mut compiler = Compiler::new(&objs);
    Ok(compiler.compile_file(&file, go_source)?)
}

/// Finds the `//export name` comment directly above `func name(`.
fn export_name(source: &str, func_name: &str) -> Option<String> {
    let lines: Vec<&str> = source.lines().collect();
    let needle = format!("func {func_name}(");
    let index = lines
        .iter()
        .position(|line| line.trim_start().starts_with(&needle))?;
    for line in lines[..index].iter().rev() {
        let line = line.trim();
        if let Some(rest) = line.strip_prefix("//export ") {
            let name = rest.trim();
            if !name.is_empty() {
                return Some(name.to_string());
            }
        }
        if !line.is_empty() && !line.starts_with("//") {
            break;
        }
    }
    None
}

fn builtin_type(name: &str) -> Option<Ty> {
    let int = |bits, signed| Some(Ty::Int { bits, signed });
    match name {
        "int" | "int64" => int(64, true),
        "int8" => int(8, true),
        "int16" => int(16, true),
        "int32" | "rune" => int(32, true),
        "uint" | "uint64" | "uintptr" => int(64, false),
        "uint8" | "byte" => int(8, false),
        "uint16" => int(16, false),
        "uint32" => int(32, false),
        "float32" => Some(Ty::Float(32)),
        "float64" => Some(Ty::Float(64)),
        "bool" => Some(Ty::Bool),
        "string" => Some(Ty::Str),
        _ => None,
    }
}

fn parse_int(lit: &str) -> R<i128> {
    let clean = lit.replace('_', "");
    let lower = clean.to_ascii_lowercase();
    let (radix, digits) = if let Some(rest) = lower.strip_prefix("0x") {
        (16, rest)
    } else if let Some(rest) = lower.strip_prefix("0b") {
        (2, rest)
    } else if let Some(rest) = lower.strip_prefix("0o") {
        (8, rest)
    } else if lower.len() > 1 && lower.starts_with('0') {
        (8, &lower[1..])
    } else {
        (10, lower.as_str())
    };
    i128::from_str_radix(digits, radix).map_err(|_| format!("invalid integer literal {lit}"))
}

/// Decodes the escapes of an interpreted string or rune literal body.
fn unescape(body: &str) -> R<Vec<u8>> {
    let mut out = Vec::new();
    let mut chars = body.chars();
    let hex = |chars: &mut std::str::Chars<'_>, n: usize| -> R<u32> {
        let digits: String = chars.by_ref().take(n).collect();
        if digits.len() != n {
            return Err(format!("invalid escape in literal {body}"));
        }
        u32::from_str_radix(&digits, 16).map_err(|_| format!("invalid escape in literal {body}"))
    };
    while let Some(c) = chars.next() {
        if c != '\\' {
            let mut buf = [0u8; 4];
            out.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
            continue;
        }
        let Some(e) = chars.next() else {
            return Err(format!("invalid escape in literal {body}"));
        };
        match e {
            'a' => out.push(7),
            'b' => out.push(8),
            'f' => out.push(12),
            'n' => out.push(b'\n'),
            'r' => out.push(b'\r'),
            't' => out.push(b'\t'),
            'v' => out.push(11),
            '\\' => out.push(b'\\'),
            '\'' => out.push(b'\''),
            '"' => out.push(b'"'),
            'x' => out.push(hex(&mut chars, 2)? as u8),
            'u' | 'U' => {
                let code = hex(&mut chars, if e == 'u' { 4 } else { 8 })?;
                let c = char::from_u32(code)
                    .ok_or_else(|| format!("invalid unicode escape in literal {body}"))?;
                let mut buf = [0u8; 4];
                out.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
            }
            '0'..='7' => {
                let mut value = e.to_digit(8).unwrap();
                for _ in 0..2 {
                    let d = chars
                        .next()
                        .and_then(|c| c.to_digit(8))
                        .ok_or_else(|| format!("invalid octal escape in literal {body}"))?;
                    value = value * 8 + d;
                }
                out.push(value as u8);
            }
            _ => return Err(format!("unknown escape \\{e} in literal {body}")),
        }
    }
    Ok(out)
}

fn unquote_string(lit: &str) -> R<Vec<u8>> {
    if let Some(raw) = lit.strip_prefix('`') {
        let raw = raw.strip_suffix('`').unwrap_or(raw);
        return Ok(raw.replace('\r', "").into_bytes());
    }
    let body = lit.strip_prefix('"').unwrap_or(lit);
    let body = body.strip_suffix('"').unwrap_or(body);
    unescape(body)
}

fn unquote_rune(lit: &str) -> R<i128> {
    let body = lit.strip_prefix('\'').unwrap_or(lit);
    let body = body.strip_suffix('\'').unwrap_or(body);
    let bytes = unescape(body)?;
    let byte_escape = body.starts_with("\\x")
        || (body.starts_with('\\') && body[1..].starts_with(|c: char| c.is_digit(8)));
    if byte_escape && bytes.len() == 1 {
        return Ok(bytes[0] as i128);
    }
    let text = String::from_utf8(bytes).map_err(|_| format!("invalid rune literal {lit}"))?;
    let mut chars = text.chars();
    match (chars.next(), chars.next()) {
        (Some(c), None) => Ok(c as i128),
        _ => Err(format!("invalid rune literal {lit}")),
    }
}

fn strip_parens(mut e: &Expr) -> &Expr {
    while let Expr::Paren(p) = e {
        e = &p.expr;
    }
    e
}

/// `sizeof(T)` as an LLVM constant expression.
fn sizeof(ty: &Ty) -> String {
    format!(
        "ptrtoint (ptr getelementptr ({}, ptr null, i32 1) to i64)",
        ty.ll()
    )
}

impl<'a> Compiler<'a> {
    fn new(objs: &'a AstObjects) -> Self {
        Compiler {
            objs,
            structs: HashMap::new(),
            struct_order: Vec::new(),
            aliases: HashMap::new(),
            funcs: HashMap::new(),
            globals: HashMap::new(),
            packages: HashSet::new(),
            strings: Vec::new(),
            string_ids: HashMap::new(),
            global_defs: Vec::new(),
            fn_defs: Vec::new(),
            f: FnCtx::new(Ty::Void, HashSet::new()),
        }
    }

    // ---- module level -----------------------------------------------------

    fn compile_file(&mut self, file: &ast::File, source: &str) -> R<String> {
        let objs = self.objs;

        for key in &file.imports {
            if let Spec::Import(import) = &objs.specs[*key] {
                let path = import
                    .path
                    .token
                    .get_literal()
                    .trim_matches('"')
                    .to_string();
                let name = match import.name {
                    Some(name) => objs.idents[name].name.clone(),
                    None => path.rsplit('/').next().unwrap_or(&path).to_string(),
                };
                self.packages.insert(name);
            }
        }

        let mut type_specs = Vec::new();
        let mut value_specs = Vec::new();
        let mut func_decls = Vec::new();
        for decl in &file.decls {
            match decl {
                Decl::Gen(g) => {
                    for key in &g.specs {
                        match &objs.specs[*key] {
                            Spec::Type(ts) => type_specs.push(ts),
                            Spec::Value(vs) => {
                                value_specs.push((vs, matches!(g.token, Token::CONST)))
                            }
                            Spec::Import(_) => {}
                        }
                    }
                }
                Decl::Func(key) => func_decls.push(&objs.fdecls[*key]),
                Decl::Bad(_) => return Err("invalid declaration".into()),
            }
        }

        // Struct names first, so types can refer to each other in any order.
        for ts in &type_specs {
            if matches!(ts.typ, Expr::Struct(_)) {
                self.declare_struct_name(&objs.idents[ts.name].name);
            }
        }
        for ts in &type_specs {
            if !matches!(ts.typ, Expr::Struct(_)) {
                let ty = self.resolve_type(&ts.typ)?;
                self.aliases.insert(objs.idents[ts.name].name.clone(), ty);
            }
        }
        for ts in &type_specs {
            if let Expr::Struct(st) = &ts.typ {
                let name = objs.idents[ts.name].name.clone();
                let fields = self.struct_fields(st)?;
                self.structs.insert(name, fields);
            }
        }

        for (vs, is_const) in value_specs {
            if is_const {
                for (name, val) in self.const_spec(vs)? {
                    self.globals.insert(name, Binding::Const(val));
                }
            } else {
                self.global_var_spec(vs)?;
            }
        }

        let mut has_main = false;
        for fd in &func_decls {
            let shape = self.func_shape(fd)?;
            let name = objs.idents[fd.name].name.as_str();
            let symbol = if shape.ptr_recv.is_some() {
                format!("main.{}", shape.key)
            } else if let Some(exported) = export_name(source, name) {
                exported
            } else {
                has_main |= name == "main";
                format!("main.{name}")
            };
            if self.funcs.contains_key(&shape.key) {
                return Err(format!("{} redeclared", shape.key));
            }
            self.funcs.insert(
                shape.key,
                FuncSig {
                    symbol,
                    params: shape.params.into_iter().map(|(_, ty)| ty).collect(),
                    ret: shape.ret,
                    ptr_recv: shape.ptr_recv,
                },
            );
        }

        for fd in &func_decls {
            self.compile_func(fd, source)?;
        }

        let mut out = String::new();
        out.push_str("; Generated by goiaba\nsource_filename = \"main.go\"\n\n");
        out.push_str("%string = type { ptr, i64 }\n%slice = type { ptr, i64, i64 }\n");
        for name in &self.struct_order {
            let fields: Vec<String> = self.structs[name].iter().map(|(_, ty)| ty.ll()).collect();
            let body = if fields.is_empty() {
                "{}".to_string()
            } else {
                format!("{{ {} }}", fields.join(", "))
            };
            let _ = writeln!(out, "{} = type {body}", Ty::Struct(name.clone()).ll());
        }
        out.push('\n');
        for (i, bytes) in self.strings.iter().enumerate() {
            let _ = writeln!(
                out,
                "@.str.{i} = private unnamed_addr constant [{} x i8] c\"{}\"",
                bytes.len(),
                c_escape(bytes)
            );
        }
        for def in &self.global_defs {
            out.push_str(def);
            out.push('\n');
        }
        out.push_str(&prelude());
        for def in &self.fn_defs {
            out.push('\n');
            out.push_str(def);
        }
        if has_main {
            out.push_str(
                "\ndefine i32 @main() {\nentry:\n  call void @\"main.main\"()\n  ret i32 0\n}\n",
            );
        }
        Ok(out)
    }

    fn declare_struct_name(&mut self, name: &str) {
        if !self.structs.contains_key(name) {
            self.structs.insert(name.to_string(), Vec::new());
            self.struct_order.push(name.to_string());
        }
    }

    fn struct_fields(&mut self, st: &ast::StructType) -> R<Vec<(String, Ty)>> {
        let objs = self.objs;
        let mut fields = Vec::new();
        for key in &st.fields.list {
            let field = &objs.fields[*key];
            if field.names.is_empty() {
                return Err("embedded struct fields are not supported yet".into());
            }
            let ty = self.resolve_type(&field.typ)?;
            for name in &field.names {
                fields.push((objs.idents[*name].name.clone(), ty.clone()));
            }
        }
        Ok(fields)
    }

    fn resolve_type(&mut self, e: &Expr) -> R<Ty> {
        let objs = self.objs;
        match e {
            Expr::Paren(p) => self.resolve_type(&p.expr),
            Expr::Ident(key) => {
                let name = objs.idents[*key].name.as_str();
                if let Some(ty) = builtin_type(name) {
                    Ok(ty)
                } else if self.structs.contains_key(name) {
                    Ok(Ty::Struct(name.to_string()))
                } else if let Some(ty) = self.aliases.get(name) {
                    Ok(ty.clone())
                } else {
                    Err(format!("undefined type: {name}"))
                }
            }
            Expr::Star(s) => Ok(Ty::Ptr(Box::new(self.resolve_type(&s.expr)?))),
            Expr::Array(a) => {
                let elem = Box::new(self.resolve_type(&a.elt)?);
                match &a.len {
                    None => Ok(Ty::Slice(elem)),
                    Some(Expr::Ellipsis(_)) => {
                        Err("[...]T is only allowed in a composite literal".into())
                    }
                    Some(len) => match self.expr(len)?.k {
                        Some(K::Int(n)) if n >= 0 => Ok(Ty::Array(elem, n as u64)),
                        _ => Err("array length must be a non-negative integer constant".into()),
                    },
                }
            }
            Expr::Struct(_) => Err("anonymous struct types are not supported yet".into()),
            Expr::Map(_) => Err("map types are not supported yet".into()),
            Expr::Chan(_) => Err("channel types are not supported yet".into()),
            Expr::Interface(_) => Err("interface types are not supported yet".into()),
            Expr::Func(_) => Err("function types are not supported yet".into()),
            Expr::Selector(_) => Err("types from other packages are not supported yet".into()),
            _ => Err("expression is not a type".into()),
        }
    }

    /// Evaluates a `const` spec. Every value must be a compile-time constant.
    fn const_spec(&mut self, vs: &ast::ValueSpec) -> R<Vec<(String, Val)>> {
        let objs = self.objs;
        if vs.values.len() != vs.names.len() {
            return Err(
                "const declarations need one value per name (iota is not supported yet)".into(),
            );
        }
        let want = match &vs.typ {
            Some(t) => Some(self.resolve_type(t)?),
            None => None,
        };
        let mut out = Vec::new();
        for (name, value) in vs.names.iter().zip(&vs.values) {
            let name = objs.idents[*name].name.clone();
            let emitted = self.f.body.len();
            let mut val = self.expr(value)?;
            if let Some(ty) = &want {
                val = self.coerce(val, ty)?;
            }
            if self.f.body.len() != emitted {
                return Err(format!("{name}: const initializer is not a constant"));
            }
            out.push((name, val));
        }
        Ok(out)
    }

    fn global_var_spec(&mut self, vs: &ast::ValueSpec) -> R<()> {
        let objs = self.objs;
        if !vs.values.is_empty() && vs.values.len() != vs.names.len() {
            return Err("package-level var needs one value per name".into());
        }
        let want = match &vs.typ {
            Some(t) => Some(self.resolve_type(t)?),
            None => None,
        };
        for (i, name) in vs.names.iter().enumerate() {
            let name = objs.idents[*name].name.clone();
            let (ty, init) = match vs.values.get(i) {
                Some(value) => {
                    let emitted = self.f.body.len();
                    let val = match &want {
                        Some(ty) => self.expr_want(value, ty)?,
                        None => self.expr_m(value)?,
                    };
                    if self.f.body.len() != emitted {
                        return Err(format!(
                            "{name}: package-level initializers must be constants for now"
                        ));
                    }
                    (val.ty, val.repr)
                }
                None => (
                    want.clone()
                        .ok_or_else(|| format!("{name}: missing type"))?,
                    "zeroinitializer".to_string(),
                ),
            };
            let symbol = format!("@\"main.{name}\"");
            self.global_defs
                .push(format!("{symbol} = internal global {} {init}", ty.ll()));
            self.globals.insert(
                name,
                Binding::Var {
                    ptr: symbol,
                    ty,
                    heap: true,
                },
            );
        }
        Ok(())
    }

    fn func_shape(&mut self, fd: &ast::FuncDecl) -> R<Shape> {
        let objs = self.objs;
        let name = objs.idents[fd.name].name.clone();
        let ft = &objs.ftypes[fd.typ];
        let mut params = Vec::new();
        let mut key = name.clone();
        let mut ptr_recv = None;

        if let Some(recv) = &fd.recv {
            let Some(field_key) = recv.list.first() else {
                return Err(format!("{name}: method has no receiver"));
            };
            let field = &objs.fields[*field_key];
            let ty = self.resolve_type(&field.typ)?;
            let (struct_name, is_ptr) = match &ty {
                Ty::Struct(n) => (n.clone(), false),
                Ty::Ptr(inner) => match inner.as_ref() {
                    Ty::Struct(n) => (n.clone(), true),
                    _ => return Err(format!("{name}: receiver must be a struct type")),
                },
                _ => return Err(format!("{name}: receiver must be a struct type")),
            };
            let recv_name = match field.names.first() {
                Some(n) => objs.idents[*n].name.clone(),
                None => "_".to_string(),
            };
            params.push((recv_name, ty));
            key = format!("{struct_name}.{name}");
            ptr_recv = Some(is_ptr);
        }

        for field_key in &ft.params.list {
            let field = &objs.fields[*field_key];
            if matches!(field.typ, Expr::Ellipsis(_)) {
                return Err(format!("{name}: variadic parameters are not supported yet"));
            }
            let ty = self.resolve_type(&field.typ)?;
            if field.names.is_empty() {
                params.push(("_".to_string(), ty));
            } else {
                for n in &field.names {
                    params.push((objs.idents[*n].name.clone(), ty.clone()));
                }
            }
        }

        let mut results = Vec::new();
        if let Some(list) = &ft.results {
            for field_key in &list.list {
                let field = &objs.fields[*field_key];
                if !field.names.is_empty() {
                    return Err(format!("{name}: named results are not supported yet"));
                }
                results.push(self.resolve_type(&field.typ)?);
            }
        }
        let ret = match results.len() {
            0 => Ty::Void,
            1 => results.pop().unwrap(),
            _ => Ty::Tuple(results),
        };

        Ok(Shape {
            key,
            params,
            ret,
            ptr_recv,
        })
    }

    fn compile_func(&mut self, fd: &ast::FuncDecl, source: &str) -> R<()> {
        let objs = self.objs;
        let name = objs.idents[fd.name].name.as_str();
        let Some(body) = &fd.body else {
            return Err(format!(
                "{name}: functions without a body are not supported"
            ));
        };
        let shape = self.func_shape(fd)?;
        let sig = self.funcs[&shape.key].clone();
        let exported = shape.ptr_recv.is_none() && export_name(source, name).is_some();

        // Lower until the set of address-taken locals stops growing.
        let mut escaping = HashSet::new();
        loop {
            self.f = FnCtx::new(shape.ret.clone(), escaping);
            self.f.scopes.push(HashMap::new());
            for (i, (pname, ty)) in shape.params.iter().enumerate() {
                if pname != "_" {
                    let arg = Val::new(ty.clone(), format!("%arg{i}"));
                    self.declare(pname, ty.clone(), Some(arg));
                }
            }
            self.block(&body.list)
                .map_err(|e| format!("{}: {e}", shape.key))?;
            if !self.f.terminated {
                if shape.ret == Ty::Void {
                    self.emit("ret void".into());
                } else {
                    self.emit("unreachable".into());
                }
            }
            if !self.f.retry {
                break;
            }
            escaping = std::mem::take(&mut self.f.escaping);
        }

        let params: Vec<String> = shape
            .params
            .iter()
            .enumerate()
            .map(|(i, (_, ty))| format!("{} %arg{i}", ty.ll()))
            .collect();
        let linkage = if exported { "" } else { "internal " };
        let mut def = format!(
            "define {linkage}{} @\"{}\"({}) {{\nentry:\n",
            shape.ret.ll(),
            sig.symbol,
            params.join(", ")
        );
        for line in self.f.allocas.iter().chain(&self.f.body) {
            def.push_str(line);
            def.push('\n');
        }
        def.push_str("}\n");
        self.fn_defs.push(def);
        Ok(())
    }

    // ---- emission helpers -------------------------------------------------

    fn fresh(&mut self) -> usize {
        self.f.counter += 1;
        self.f.counter
    }

    fn new_label(&mut self, hint: &str) -> String {
        let n = self.fresh();
        format!("{hint}.{n}")
    }

    fn emit(&mut self, line: String) {
        if self.f.terminated {
            // Code after a terminator is unreachable; give it its own block.
            let label = self.new_label("dead");
            self.f.body.push(format!("{label}:"));
            self.f.cur = label;
            self.f.terminated = false;
        }
        self.f.body.push(format!("  {line}"));
    }

    /// Emits `%tN = <rhs>` and returns the new name.
    fn ins(&mut self, rhs: String) -> String {
        let name = format!("%t{}", self.fresh());
        self.emit(format!("{name} = {rhs}"));
        name
    }

    fn br(&mut self, label: &str) {
        if !self.f.terminated {
            self.emit(format!("br label %{label}"));
            self.f.terminated = true;
        }
    }

    fn cond_br(&mut self, cond: &str, then: &str, els: &str) {
        self.emit(format!("br i1 {cond}, label %{then}, label %{els}"));
        self.f.terminated = true;
    }

    fn place_label(&mut self, label: &str) {
        if !self.f.terminated {
            self.f.body.push(format!("  br label %{label}"));
        }
        self.f.body.push(format!("{label}:"));
        self.f.cur = label.to_string();
        self.f.terminated = false;
    }

    fn alloca(&mut self, ty: &Ty) -> String {
        let name = format!("%a{}", self.fresh());
        self.f
            .allocas
            .push(format!("  {name} = alloca {}", ty.ll()));
        name
    }

    fn heap_alloc(&mut self, ty: &Ty) -> String {
        self.ins(format!("call ptr @calloc(i64 1, i64 {})", sizeof(ty)))
    }

    fn store(&mut self, val: &Val, ptr: &str) {
        self.emit(format!("store {} {}, ptr {ptr}", val.ty.ll(), val.repr));
    }

    fn load(&mut self, ptr: &str, ty: &Ty) -> Val {
        let name = self.ins(format!("load {}, ptr {ptr}", ty.ll()));
        Val::new(ty.clone(), name)
    }

    /// Copies a value into a stack slot and returns the slot.
    fn spill(&mut self, val: &Val) -> String {
        let slot = self.alloca(&val.ty);
        self.store(val, &slot);
        slot
    }

    fn intern(&mut self, bytes: Vec<u8>) -> String {
        let id = match self.string_ids.get(&bytes) {
            Some(id) => *id,
            None => {
                let id = self.strings.len();
                self.strings.push(bytes.clone());
                self.string_ids.insert(bytes, id);
                id
            }
        };
        format!("@.str.{id}")
    }

    /// A NUL-terminated constant for libc calls.
    fn cstr(&mut self, text: &str) -> String {
        let mut bytes = text.as_bytes().to_vec();
        bytes.push(0);
        self.intern(bytes)
    }

    fn string_const(&mut self, bytes: Vec<u8>) -> Val {
        if bytes.is_empty() {
            return Val::new(Ty::Str, "zeroinitializer");
        }
        let len = bytes.len();
        let global = self.intern(bytes);
        Val::new(Ty::Str, format!("{{ ptr {global}, i64 {len} }}"))
    }

    fn panic_if(&mut self, cond: &str, message: &str) {
        let msg = self.cstr(message);
        self.emit(format!("call void @goiaba.panic_if(i1 {cond}, ptr {msg})"));
    }

    // ---- scopes -----------------------------------------------------------

    fn push_scope(&mut self) {
        self.f.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.f.scopes.pop();
    }

    fn lookup(&self, name: &str) -> Option<Binding> {
        self.f
            .scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name))
            .or_else(|| self.globals.get(name))
            .cloned()
    }

    fn bind(&mut self, name: &str, binding: Binding) {
        if name == "_" {
            return;
        }
        match self.f.scopes.last_mut() {
            Some(scope) => {
                scope.insert(name.to_string(), binding);
            }
            None => {
                self.globals.insert(name.to_string(), binding);
            }
        }
    }

    /// Declares a local. Without `init` the variable holds its zero value.
    fn declare(&mut self, name: &str, ty: Ty, init: Option<Val>) {
        let heap = self.f.escaping.contains(name);
        let ptr = if heap {
            self.heap_alloc(&ty)
        } else {
            self.alloca(&ty)
        };
        match init {
            Some(val) => self.store(&val, &ptr),
            None if !heap => self.store(&Val::new(ty.clone(), "zeroinitializer"), &ptr),
            None => {}
        }
        self.bind(name, Binding::Var { ptr, ty, heap });
    }

    /// Records that the variable at the root of `e` has its address taken.
    fn mark_escape(&mut self, e: &Expr) {
        let objs = self.objs;
        let mut e = e;
        let name = loop {
            match e {
                Expr::Paren(p) => e = &p.expr,
                Expr::Selector(s) => e = &s.expr,
                Expr::Index(ix) => e = &ix.expr,
                Expr::Ident(key) => break objs.idents[*key].name.as_str(),
                _ => return,
            }
        };
        if let Some(Binding::Var { heap: false, .. }) = self.lookup(name)
            && self.f.escaping.insert(name.to_string())
        {
            self.f.retry = true;
        }
    }

    // ---- conversions ------------------------------------------------------

    /// Gives an untyped constant its default type.
    fn materialize(&mut self, val: Val) -> R<Val> {
        match val.ty {
            Ty::UntypedInt => self.convert(val, &INT),
            Ty::UntypedRune => self.convert(
                val,
                &Ty::Int {
                    bits: 32,
                    signed: true,
                },
            ),
            Ty::UntypedFloat => self.convert(val, &Ty::Float(64)),
            Ty::Nil => Err("use of untyped nil".into()),
            Ty::Void => Err("expression has no value".into()),
            _ => Ok(val),
        }
    }

    /// Implicit conversion for assignment: only untyped constants and nil change type.
    fn coerce(&mut self, val: Val, to: &Ty) -> R<Val> {
        if val.ty == *to {
            return Ok(val);
        }
        if val.ty.is_untyped_num() {
            return self.convert(val, to);
        }
        match (&val.ty, to) {
            (Ty::Nil, Ty::Ptr(_)) => Ok(Val::new(to.clone(), "null")),
            (Ty::Nil, Ty::Slice(_)) => Ok(Val::new(to.clone(), "zeroinitializer")),
            _ => Err(format!("cannot use {} as {to}", val.ty)),
        }
    }

    /// Explicit conversion `T(x)`.
    fn convert(&mut self, val: Val, to: &Ty) -> R<Val> {
        if val.ty == *to {
            return Ok(val);
        }
        if val.ty.is_untyped_num() {
            let k = val.k.ok_or("internal: untyped value without a constant")?;
            return match (to, k) {
                (Ty::Int { bits, signed }, K::Int(v)) => {
                    let (min, max) = if *signed {
                        (-(1i128 << (bits - 1)), (1i128 << (bits - 1)) - 1)
                    } else {
                        (0, (1i128 << bits) - 1)
                    };
                    if v < min || v > max {
                        return Err(format!("constant {v} overflows {to}"));
                    }
                    Ok(Val::int(to.clone(), v))
                }
                (Ty::Int { .. }, K::Float(v)) => {
                    if v.fract() != 0.0 {
                        return Err(format!("constant {v} truncated to integer"));
                    }
                    self.convert(Val::int(Ty::UntypedInt, v as i128), to)
                }
                (Ty::Float(_), k) => Ok(Val::float(to.clone(), k.as_f64())),
                _ => Err(format!("cannot convert {} constant to {to}", val.ty)),
            };
        }
        let (from_ll, to_ll) = (val.ty.ll(), to.ll());
        let cast = match (&val.ty, to) {
            (Ty::Int { bits: from, signed }, Ty::Int { bits: into, .. }) => {
                if from == into {
                    return Ok(Val {
                        ty: to.clone(),
                        ..val
                    });
                }
                if from > into {
                    "trunc"
                } else if *signed {
                    "sext"
                } else {
                    "zext"
                }
            }
            (Ty::Int { signed, .. }, Ty::Float(_)) => {
                if *signed {
                    "sitofp"
                } else {
                    "uitofp"
                }
            }
            (Ty::Float(_), Ty::Int { signed, .. }) => {
                if *signed {
                    "fptosi"
                } else {
                    "fptoui"
                }
            }
            (Ty::Float(from), Ty::Float(into)) => {
                if from < into {
                    "fpext"
                } else {
                    "fptrunc"
                }
            }
            _ => return Err(format!("cannot convert {} to {to}", val.ty)),
        };
        let name = self.ins(format!("{cast} {from_ll} {} to {to_ll}", val.repr));
        Ok(Val::new(to.clone(), name))
    }

    /// Widens an integer operand to i64, for indexes and lengths.
    fn widen_i64(&mut self, val: Val) -> R<String> {
        let val = self.materialize(val)?;
        match val.ty {
            Ty::Int { .. } => Ok(self.convert(val, &INT)?.repr),
            other => Err(format!("expected an integer, found {other}")),
        }
    }

    // ---- expressions ------------------------------------------------------

    /// Evaluates an expression. The result may be an untyped constant.
    fn expr(&mut self, e: &Expr) -> R<Val> {
        match self.loc(e)? {
            Loc::Place(ptr, ty) => Ok(self.load(&ptr, &ty)),
            Loc::Value(val) => Ok(val),
        }
    }

    /// Evaluates an expression and gives untyped constants their default type.
    fn expr_m(&mut self, e: &Expr) -> R<Val> {
        let val = self.expr(e)?;
        self.materialize(val)
    }

    /// Evaluates an expression that must be assignable to `want`.
    fn expr_want(&mut self, e: &Expr, want: &Ty) -> R<Val> {
        let val = match strip_parens(e) {
            Expr::CompositeLit(lit) if lit.typ.is_none() => self.composite(lit, Some(want))?,
            _ => self.expr(e)?,
        };
        self.coerce(val, want)
    }

    fn loc(&mut self, e: &Expr) -> R<Loc> {
        let objs = self.objs;
        match e {
            Expr::Paren(p) => self.loc(&p.expr),
            Expr::Ident(key) => {
                let name = objs.idents[*key].name.as_str();
                match self.lookup(name) {
                    Some(Binding::Var { ptr, ty, .. }) => Ok(Loc::Place(ptr, ty)),
                    Some(Binding::Const(val)) => Ok(Loc::Value(val)),
                    None => match name {
                        "true" => Ok(Loc::Value(Val::bool(true))),
                        "false" => Ok(Loc::Value(Val::bool(false))),
                        "nil" => Ok(Loc::Value(Val::new(Ty::Nil, "null"))),
                        "_" => Err("cannot use _ as a value".into()),
                        _ if self.funcs.contains_key(name) => {
                            Err(format!("{name}: function values are not supported yet"))
                        }
                        _ => Err(format!("undefined: {name}")),
                    },
                }
            }
            Expr::Star(s) => self.deref(&s.expr),
            Expr::Unary(u) if matches!(u.op, Token::MUL) => self.deref(&u.expr),
            Expr::Selector(s) => self.field(s),
            Expr::Index(ix) => self.index(ix),
            _ => Ok(Loc::Value(self.rvalue(e)?)),
        }
    }

    fn deref(&mut self, e: &Expr) -> R<Loc> {
        let val = self.expr(e)?;
        match val.ty {
            Ty::Ptr(inner) => Ok(Loc::Place(val.repr, *inner)),
            other => Err(format!("cannot dereference {other}")),
        }
    }

    /// Returns a pointer to the struct `e` denotes, following one pointer level.
    fn struct_ptr(&mut self, e: &Expr) -> R<(String, String)> {
        let val = match self.loc(e)? {
            Loc::Place(ptr, Ty::Struct(name)) => return Ok((ptr, name)),
            Loc::Place(ptr, ty) => self.load(&ptr, &ty),
            Loc::Value(val) => val,
        };
        match &val.ty {
            Ty::Ptr(inner) => match inner.as_ref() {
                Ty::Struct(name) => Ok((val.repr.clone(), name.clone())),
                other => Err(format!("*{other} has no fields")),
            },
            Ty::Struct(name) => Ok((self.spill(&val), name.clone())),
            other => Err(format!("{other} has no fields")),
        }
    }

    fn field(&mut self, s: &ast::SelectorExpr) -> R<Loc> {
        let objs = self.objs;
        let sel = objs.idents[s.sel].name.as_str();
        self.check_package(&s.expr)?;
        let (ptr, struct_name) = self.struct_ptr(&s.expr)?;
        let fields = &self.structs[&struct_name];
        let Some(index) = fields.iter().position(|(name, _)| name == sel) else {
            return Err(format!("{struct_name} has no field {sel}"));
        };
        let field_ty = fields[index].1.clone();
        let place = self.ins(format!(
            "getelementptr inbounds {}, ptr {ptr}, i32 0, i32 {index}",
            Ty::Struct(struct_name).ll()
        ));
        Ok(Loc::Place(place, field_ty))
    }

    /// Rejects `pkg.Name`: no imported package is available to this backend yet.
    fn check_package(&self, e: &Expr) -> R<()> {
        if let Expr::Ident(key) = strip_parens(e) {
            let name = self.objs.idents[*key].name.as_str();
            if self.lookup(name).is_none() && self.packages.contains(name) {
                return Err(format!(
                    "package {name} is not available in the LLVM backend yet"
                ));
            }
        }
        Ok(())
    }

    fn index(&mut self, ix: &ast::IndexExpr) -> R<Loc> {
        let base = self.loc(&ix.expr)?;
        let idx_val = self.expr(&ix.index)?;
        let idx = self.widen_i64(idx_val)?;

        let array_at = |c: &mut Self, ptr: &str, elem: &Ty, n: u64| {
            c.emit(format!("call void @goiaba.bounds(i64 {idx}, i64 {n})"));
            let place = c.ins(format!(
                "getelementptr inbounds [{n} x {}], ptr {ptr}, i64 0, i64 {idx}",
                elem.ll()
            ));
            Loc::Place(place, elem.clone())
        };

        let val = match base {
            Loc::Place(ptr, Ty::Array(elem, n)) => return Ok(array_at(self, &ptr, &elem, n)),
            Loc::Place(ptr, ty) => self.load(&ptr, &ty),
            Loc::Value(val) => val,
        };
        match &val.ty {
            Ty::Slice(elem) => {
                let data = self.ins(format!("extractvalue %slice {}, 0", val.repr));
                let len = self.ins(format!("extractvalue %slice {}, 1", val.repr));
                self.emit(format!("call void @goiaba.bounds(i64 {idx}, i64 {len})"));
                let place = self.ins(format!(
                    "getelementptr inbounds {}, ptr {data}, i64 {idx}",
                    elem.ll()
                ));
                Ok(Loc::Place(place, elem.as_ref().clone()))
            }
            Ty::Ptr(inner) => match inner.as_ref() {
                Ty::Array(elem, n) => Ok(array_at(self, &val.repr, elem, *n)),
                other => Err(format!("cannot index *{other}")),
            },
            Ty::Array(elem, n) => {
                let slot = self.spill(&val);
                Ok(array_at(self, &slot, elem, *n))
            }
            Ty::Str => {
                let data = self.ins(format!("extractvalue %string {}, 0", val.repr));
                let len = self.ins(format!("extractvalue %string {}, 1", val.repr));
                self.emit(format!("call void @goiaba.bounds(i64 {idx}, i64 {len})"));
                let place = self.ins(format!("getelementptr inbounds i8, ptr {data}, i64 {idx}"));
                let byte = Ty::Int {
                    bits: 8,
                    signed: false,
                };
                Ok(Loc::Value(self.load(&place, &byte)))
            }
            other => Err(format!("cannot index {other}")),
        }
    }

    fn rvalue(&mut self, e: &Expr) -> R<Val> {
        match e {
            Expr::BasicLit(lit) => self.literal(&lit.token),
            Expr::Binary(b) => match b.op {
                Token::LAND => self.logical(b, true),
                Token::LOR => self.logical(b, false),
                _ => {
                    let op = Op::from_binary(&b.op)
                        .ok_or_else(|| format!("unsupported binary operator {:?}", b.op))?;
                    let left = self.expr(&b.expr_a)?;
                    let right = self.expr(&b.expr_b)?;
                    self.binary(op, left, right)
                }
            },
            Expr::Unary(u) => self.unary(u),
            Expr::Call(call) => self.call(call),
            Expr::CompositeLit(lit) => self.composite(lit, None),
            Expr::Slice(s) => self.slice_expr(s),
            Expr::FuncLit(_) => Err("function literals are not supported yet".into()),
            Expr::TypeAssert(_) => Err("type assertions are not supported yet".into()),
            Expr::KeyValue(_) => Err("unexpected key: value pair".into()),
            _ => Err("unsupported expression".into()),
        }
    }

    fn literal(&mut self, token: &Token) -> R<Val> {
        let text = token.get_literal();
        match token {
            Token::INT(_) => Ok(Val::int(Ty::UntypedInt, parse_int(text)?)),
            Token::CHAR(_) => Ok(Val::int(Ty::UntypedRune, unquote_rune(text)?)),
            Token::FLOAT(_) => {
                let v: f64 = text
                    .replace('_', "")
                    .parse()
                    .map_err(|_| format!("invalid float literal {text}"))?;
                Ok(Val::float(Ty::UntypedFloat, v))
            }
            Token::STRING(_) => {
                let bytes = unquote_string(text)?;
                Ok(self.string_const(bytes))
            }
            _ => Err(format!("unsupported literal {text}")),
        }
    }

    /// `&&` and `||`: the right side runs only when the left does not decide.
    fn logical(&mut self, b: &ast::BinaryExpr, is_and: bool) -> R<Val> {
        let left = self.expr_want(&b.expr_a, &Ty::Bool)?;
        let left_block = self.f.cur.clone();
        let rhs = self.new_label("logic.rhs");
        let end = self.new_label("logic.end");
        if is_and {
            self.cond_br(&left.repr, &rhs, &end);
        } else {
            self.cond_br(&left.repr, &end, &rhs);
        }
        self.place_label(&rhs);
        let right = self.expr_want(&b.expr_b, &Ty::Bool)?;
        let right_block = self.f.cur.clone();
        self.br(&end);
        self.place_label(&end);
        let short = if is_and { "false" } else { "true" };
        let name = self.ins(format!(
            "phi i1 [ {short}, %{left_block} ], [ {}, %{right_block} ]",
            right.repr
        ));
        Ok(Val::new(Ty::Bool, name))
    }

    fn unary(&mut self, u: &ast::UnaryExpr) -> R<Val> {
        if matches!(u.op, Token::AND) {
            if let Expr::CompositeLit(lit) = strip_parens(&u.expr) {
                let val = self.composite(lit, None)?;
                let ptr = self.heap_alloc(&val.ty);
                self.store(&val, &ptr);
                return Ok(Val::new(Ty::Ptr(Box::new(val.ty)), ptr));
            }
            self.mark_escape(&u.expr);
            return match self.loc(&u.expr)? {
                Loc::Place(ptr, ty) => Ok(Val::new(Ty::Ptr(Box::new(ty)), ptr)),
                Loc::Value(_) => Err("cannot take the address of this expression".into()),
            };
        }

        let val = self.expr(&u.expr)?;
        match (&u.op, &val.ty, val.k) {
            (Token::ADD, ty, _)
                if ty.is_untyped_num() || matches!(ty, Ty::Int { .. } | Ty::Float(_)) =>
            {
                Ok(val)
            }
            (Token::SUB, ty, Some(K::Int(v))) if ty.is_untyped_num() => {
                Ok(Val::int(val.ty.clone(), -v))
            }
            (Token::SUB, ty, Some(K::Float(v))) if ty.is_untyped_num() => {
                Ok(Val::float(val.ty.clone(), -v))
            }
            (Token::XOR, ty, Some(K::Int(v))) if ty.is_untyped_num() => {
                Ok(Val::int(val.ty.clone(), !v))
            }
            (Token::SUB, Ty::Int { .. }, _) => {
                let name = self.ins(format!("sub {} 0, {}", val.ty.ll(), val.repr));
                Ok(Val::new(val.ty, name))
            }
            (Token::SUB, Ty::Float(_), _) => {
                let name = self.ins(format!("fneg {} {}", val.ty.ll(), val.repr));
                Ok(Val::new(val.ty, name))
            }
            (Token::XOR, Ty::Int { .. }, _) => {
                let name = self.ins(format!("xor {} {}, -1", val.ty.ll(), val.repr));
                Ok(Val::new(val.ty, name))
            }
            (Token::NOT, Ty::Bool, _) => {
                let name = self.ins(format!("xor i1 {}, true", val.repr));
                Ok(Val::new(Ty::Bool, name))
            }
            (op, ty, _) => Err(format!("unsupported unary operator {op:?} on {ty}")),
        }
    }

    fn binary(&mut self, op: Op, left: Val, right: Val) -> R<Val> {
        if matches!(op, Op::Shl | Op::Shr) {
            return self.shift(op, left, right);
        }
        if left.ty.is_untyped_num() && right.ty.is_untyped_num() {
            return fold(op, &left, &right);
        }

        if left.ty == Ty::Nil || right.ty == Ty::Nil {
            let other = if left.ty == Ty::Nil { &right } else { &left };
            let ptr = match &other.ty {
                Ty::Ptr(_) => other.repr.clone(),
                Ty::Slice(_) => self.ins(format!("extractvalue %slice {}, 0", other.repr)),
                ty => return Err(format!("cannot compare {ty} with nil")),
            };
            let pred = match op {
                Op::Eq => "eq",
                Op::Ne => "ne",
                _ => return Err(format!("operator {} is not defined on nil", op.symbol())),
            };
            let name = self.ins(format!("icmp {pred} ptr {ptr}, null"));
            return Ok(Val::new(Ty::Bool, name));
        }

        let (left, right) = if left.ty.is_untyped_num() {
            let ty = right.ty.clone();
            (self.coerce(left, &ty)?, right)
        } else if right.ty.is_untyped_num() {
            let ty = left.ty.clone();
            let right = self.coerce(right, &ty)?;
            (left, right)
        } else {
            (left, right)
        };
        if left.ty != right.ty {
            return Err(format!(
                "mismatched types {} and {} for operator {}",
                left.ty,
                right.ty,
                op.symbol()
            ));
        }

        let ty = left.ty.clone();
        let ll = ty.ll();
        let (a, b) = (left.repr.as_str(), right.repr.as_str());
        let undefined = || format!("operator {} is not defined on {ty}", op.symbol());

        match &ty {
            Ty::Int { signed, .. } => {
                if op.is_comparison() {
                    let pred = match (op, signed) {
                        (Op::Eq, _) => "eq",
                        (Op::Ne, _) => "ne",
                        (Op::Lt, true) => "slt",
                        (Op::Le, true) => "sle",
                        (Op::Gt, true) => "sgt",
                        (Op::Ge, true) => "sge",
                        (Op::Lt, false) => "ult",
                        (Op::Le, false) => "ule",
                        (Op::Gt, false) => "ugt",
                        _ => "uge",
                    };
                    let name = self.ins(format!("icmp {pred} {ll} {a}, {b}"));
                    return Ok(Val::new(Ty::Bool, name));
                }
                let name = match op {
                    Op::Add => self.ins(format!("add {ll} {a}, {b}")),
                    Op::Sub => self.ins(format!("sub {ll} {a}, {b}")),
                    Op::Mul => self.ins(format!("mul {ll} {a}, {b}")),
                    Op::And => self.ins(format!("and {ll} {a}, {b}")),
                    Op::Or => self.ins(format!("or {ll} {a}, {b}")),
                    Op::Xor => self.ins(format!("xor {ll} {a}, {b}")),
                    Op::AndNot => {
                        let inv = self.ins(format!("xor {ll} {b}, -1"));
                        self.ins(format!("and {ll} {a}, {inv}"))
                    }
                    Op::Div | Op::Rem => self.int_div(op, &left, &right, *signed)?,
                    _ => return Err(undefined()),
                };
                Ok(Val::new(ty, name))
            }
            Ty::Float(_) => {
                if op.is_comparison() {
                    let pred = match op {
                        Op::Eq => "oeq",
                        Op::Ne => "une",
                        Op::Lt => "olt",
                        Op::Le => "ole",
                        Op::Gt => "ogt",
                        _ => "oge",
                    };
                    let name = self.ins(format!("fcmp {pred} {ll} {a}, {b}"));
                    return Ok(Val::new(Ty::Bool, name));
                }
                let inst = match op {
                    Op::Add => "fadd",
                    Op::Sub => "fsub",
                    Op::Mul => "fmul",
                    Op::Div => "fdiv",
                    _ => return Err(undefined()),
                };
                let name = self.ins(format!("{inst} {ll} {a}, {b}"));
                Ok(Val::new(ty, name))
            }
            Ty::Bool | Ty::Ptr(_) => {
                let pred = match op {
                    Op::Eq => "eq",
                    Op::Ne => "ne",
                    _ => return Err(undefined()),
                };
                let name = self.ins(format!("icmp {pred} {ll} {a}, {b}"));
                Ok(Val::new(Ty::Bool, name))
            }
            Ty::Str => {
                if op == Op::Add {
                    let name = self.ins(format!(
                        "call %string @goiaba.concat(%string {a}, %string {b})"
                    ));
                    return Ok(Val::new(Ty::Str, name));
                }
                let pred = match op {
                    Op::Eq => "eq",
                    Op::Ne => "ne",
                    Op::Lt => "slt",
                    Op::Le => "sle",
                    Op::Gt => "sgt",
                    Op::Ge => "sge",
                    _ => return Err(undefined()),
                };
                let cmp = self.ins(format!("call i32 @goiaba.strcmp(%string {a}, %string {b})"));
                let name = self.ins(format!("icmp {pred} i32 {cmp}, 0"));
                Ok(Val::new(Ty::Bool, name))
            }
            _ => Err(undefined()),
        }
    }

    /// Integer `/` and `%` with Go's semantics: a zero divisor panics and
    /// `MinInt / -1` wraps. Both are undefined behaviour for LLVM's sdiv/srem.
    fn int_div(&mut self, op: Op, left: &Val, right: &Val, signed: bool) -> R<String> {
        let ll = left.ty.ll();
        let (a, b) = (left.repr.as_str(), right.repr.as_str());
        let inst = match (op, signed) {
            (Op::Div, true) => "sdiv",
            (Op::Div, false) => "udiv",
            (_, true) => "srem",
            (_, false) => "urem",
        };
        match right.k {
            Some(K::Int(0)) => return Err("division by zero".into()),
            Some(K::Int(-1)) if signed => {
                return Ok(if op == Op::Div {
                    self.ins(format!("sub {ll} 0, {a}"))
                } else {
                    "0".to_string()
                });
            }
            Some(_) => return Ok(self.ins(format!("{inst} {ll} {a}, {b}"))),
            None => {}
        }
        let is_zero = self.ins(format!("icmp eq {ll} {b}, 0"));
        self.panic_if(&is_zero, "integer divide by zero");
        if !signed {
            return Ok(self.ins(format!("{inst} {ll} {a}, {b}")));
        }
        let minus_one = self.ins(format!("icmp eq {ll} {b}, -1"));
        let safe = self.ins(format!("select i1 {minus_one}, {ll} 1, {ll} {b}"));
        let raw = self.ins(format!("{inst} {ll} {a}, {safe}"));
        let wrapped = if op == Op::Div {
            self.ins(format!("sub {ll} 0, {a}"))
        } else {
            "0".to_string()
        };
        Ok(self.ins(format!("select i1 {minus_one}, {ll} {wrapped}, {ll} {raw}")))
    }

    /// Shifts with Go's semantics: a count of at least the operand width
    /// gives 0 (or the sign fill), where LLVM would give poison.
    fn shift(&mut self, op: Op, left: Val, right: Val) -> R<Val> {
        if left.ty.is_untyped_num()
            && let (Some(K::Int(a)), Some(K::Int(n))) = (left.k, right.k)
        {
            if !(0..=100).contains(&n) {
                return Err(format!("invalid shift count {n}"));
            }
            let v = if op == Op::Shl { a << n } else { a >> n };
            return Ok(Val::int(left.ty, v));
        }
        let left = self.materialize(left)?;
        let Ty::Int { bits, signed } = left.ty else {
            return Err(format!("cannot shift {}", left.ty));
        };
        let ll = left.ty.ll();
        let inst = match (op, signed) {
            (Op::Shl, _) => "shl",
            (_, true) => "ashr",
            (_, false) => "lshr",
        };

        if right.ty.is_untyped_num() || right.k.is_some() {
            let Some(K::Int(n)) = right.k else {
                return Err("shift count must be an integer".into());
            };
            if n < 0 {
                return Err(format!("negative shift count {n}"));
            }
            if n >= bits as i128 {
                if inst != "ashr" {
                    return Ok(Val::int(left.ty, 0));
                }
                let name = self.ins(format!("ashr {ll} {}, {}", left.repr, bits - 1));
                return Ok(Val::new(left.ty, name));
            }
            let name = self.ins(format!("{inst} {ll} {}, {n}", left.repr));
            return Ok(Val::new(left.ty, name));
        }

        let Ty::Int {
            bits: count_bits, ..
        } = right.ty
        else {
            return Err(format!(
                "shift count must be an integer, found {}",
                right.ty
            ));
        };
        let count_ll = right.ty.ll();
        let too_big = self.ins(format!("icmp uge {count_ll} {}, {bits}", right.repr));
        let count = if count_bits > bits {
            self.ins(format!("trunc {count_ll} {} to {ll}", right.repr))
        } else if count_bits < bits {
            self.ins(format!("zext {count_ll} {} to {ll}", right.repr))
        } else {
            right.repr.clone()
        };
        let name = if inst == "ashr" {
            let clamped = self.ins(format!(
                "select i1 {too_big}, {ll} {}, {ll} {count}",
                bits - 1
            ));
            self.ins(format!("ashr {ll} {}, {clamped}", left.repr))
        } else {
            let raw = self.ins(format!("{inst} {ll} {}, {count}", left.repr));
            self.ins(format!("select i1 {too_big}, {ll} 0, {ll} {raw}"))
        };
        Ok(Val::new(left.ty, name))
    }

    fn composite(&mut self, lit: &ast::CompositeLit, want: Option<&Ty>) -> R<Val> {
        let objs = self.objs;
        let ty = match &lit.typ {
            Some(Expr::Array(a)) if matches!(a.len, Some(Expr::Ellipsis(_))) => {
                Ty::Array(Box::new(self.resolve_type(&a.elt)?), lit.elts.len() as u64)
            }
            Some(t) => self.resolve_type(t)?,
            None => want
                .cloned()
                .ok_or("composite literal is missing its type")?,
        };
        match &ty {
            Ty::Struct(name) => {
                let fields = self.structs[name].clone();
                let ll = ty.ll();
                let mut cur = "zeroinitializer".to_string();
                for (i, elt) in lit.elts.iter().enumerate() {
                    let (index, value) = match elt {
                        Expr::KeyValue(kv) => {
                            let Expr::Ident(key) = &kv.key else {
                                return Err(format!("invalid field name in {name} literal"));
                            };
                            let field = objs.idents[*key].name.as_str();
                            let index = fields
                                .iter()
                                .position(|(n, _)| n == field)
                                .ok_or_else(|| format!("{name} has no field {field}"))?;
                            (index, &kv.val)
                        }
                        other => (i, other),
                    };
                    let Some((_, field_ty)) = fields.get(index) else {
                        return Err(format!("too many values in {name} literal"));
                    };
                    let val = self.expr_want(value, field_ty)?;
                    cur = self.ins(format!(
                        "insertvalue {ll} {cur}, {} {}, {index}",
                        field_ty.ll(),
                        val.repr
                    ));
                }
                Ok(Val::new(ty, cur))
            }
            Ty::Array(elem, n) => {
                if lit.elts.len() as u64 > *n {
                    return Err(format!("too many values in {ty} literal"));
                }
                let ll = ty.ll();
                let mut cur = "zeroinitializer".to_string();
                for (i, elt) in lit.elts.iter().enumerate() {
                    if matches!(elt, Expr::KeyValue(_)) {
                        return Err("indexed array literals are not supported yet".into());
                    }
                    let val = self.expr_want(elt, elem)?;
                    cur = self.ins(format!(
                        "insertvalue {ll} {cur}, {} {}, {i}",
                        elem.ll(),
                        val.repr
                    ));
                }
                Ok(Val::new(ty, cur))
            }
            Ty::Slice(elem) => {
                let n = lit.elts.len();
                if n == 0 {
                    return Ok(Val::new(ty, "zeroinitializer"));
                }
                let data = self.ins(format!("call ptr @calloc(i64 {n}, i64 {})", sizeof(elem)));
                for (i, elt) in lit.elts.iter().enumerate() {
                    if matches!(elt, Expr::KeyValue(_)) {
                        return Err("indexed slice literals are not supported yet".into());
                    }
                    let val = self.expr_want(elt, elem)?;
                    let slot = self.ins(format!(
                        "getelementptr inbounds {}, ptr {data}, i64 {i}",
                        elem.ll()
                    ));
                    self.store(&val, &slot);
                }
                let n = n.to_string();
                Ok(self.make_slice(ty.clone(), &data, &n, &n))
            }
            Ty::Ptr(inner) if lit.typ.is_none() => {
                // Elided `&T` inside a literal such as `[]*T{{...}}`.
                let val = self.composite(lit, Some(inner))?;
                let ptr = self.heap_alloc(&val.ty);
                self.store(&val, &ptr);
                Ok(Val::new(ty.clone(), ptr))
            }
            other => Err(format!("invalid composite literal type {other}")),
        }
    }

    fn make_slice(&mut self, ty: Ty, data: &str, len: &str, cap: &str) -> Val {
        let a = self.ins(format!("insertvalue %slice poison, ptr {data}, 0"));
        let b = self.ins(format!("insertvalue %slice {a}, i64 {len}, 1"));
        let c = self.ins(format!("insertvalue %slice {b}, i64 {cap}, 2"));
        Val::new(ty, c)
    }

    fn slice_expr(&mut self, s: &ast::SliceExpr) -> R<Val> {
        if s.slice3 || s.max.is_some() {
            return Err("3-index slices are not supported yet".into());
        }
        let base = self.loc(&s.expr)?;
        let lo = match &s.low {
            Some(e) => {
                let v = self.expr(e)?;
                self.widen_i64(v)?
            }
            None => "0".to_string(),
        };
        let high = match &s.high {
            Some(e) => {
                let v = self.expr(e)?;
                Some(self.widen_i64(v)?)
            }
            None => None,
        };

        let from_array = |c: &mut Self, ptr: &str, elem: &Ty, n: u64, high: Option<String>| {
            let hi = high.unwrap_or_else(|| n.to_string());
            c.emit(format!(
                "call void @goiaba.slicecheck(i64 {lo}, i64 {hi}, i64 {n})"
            ));
            let data = c.ins(format!(
                "getelementptr inbounds {}, ptr {ptr}, i64 {lo}",
                elem.ll()
            ));
            let len = c.ins(format!("sub i64 {hi}, {lo}"));
            let cap = c.ins(format!("sub i64 {n}, {lo}"));
            c.make_slice(Ty::Slice(Box::new(elem.clone())), &data, &len, &cap)
        };

        let val = match base {
            Loc::Place(ptr, Ty::Array(elem, n)) => {
                self.mark_escape(&s.expr);
                return Ok(from_array(self, &ptr, &elem, n, high));
            }
            Loc::Place(ptr, ty) => self.load(&ptr, &ty),
            Loc::Value(val) => val,
        };
        match &val.ty {
            Ty::Slice(elem) => {
                let data = self.ins(format!("extractvalue %slice {}, 0", val.repr));
                let cap = self.ins(format!("extractvalue %slice {}, 2", val.repr));
                let hi = match high {
                    Some(hi) => hi,
                    None => self.ins(format!("extractvalue %slice {}, 1", val.repr)),
                };
                self.emit(format!(
                    "call void @goiaba.slicecheck(i64 {lo}, i64 {hi}, i64 {cap})"
                ));
                let new_data = self.ins(format!(
                    "getelementptr inbounds {}, ptr {data}, i64 {lo}",
                    elem.ll()
                ));
                let len = self.ins(format!("sub i64 {hi}, {lo}"));
                let new_cap = self.ins(format!("sub i64 {cap}, {lo}"));
                Ok(self.make_slice(val.ty.clone(), &new_data, &len, &new_cap))
            }
            Ty::Str => {
                let data = self.ins(format!("extractvalue %string {}, 0", val.repr));
                let len = self.ins(format!("extractvalue %string {}, 1", val.repr));
                let hi = high.unwrap_or_else(|| len.clone());
                self.emit(format!(
                    "call void @goiaba.slicecheck(i64 {lo}, i64 {hi}, i64 {len})"
                ));
                let new_data = self.ins(format!("getelementptr inbounds i8, ptr {data}, i64 {lo}"));
                let new_len = self.ins(format!("sub i64 {hi}, {lo}"));
                let a = self.ins(format!("insertvalue %string poison, ptr {new_data}, 0"));
                let b = self.ins(format!("insertvalue %string {a}, i64 {new_len}, 1"));
                Ok(Val::new(Ty::Str, b))
            }
            Ty::Ptr(inner) => match inner.as_ref() {
                Ty::Array(elem, n) => Ok(from_array(self, &val.repr, elem, *n, high)),
                other => Err(format!("cannot slice *{other}")),
            },
            Ty::Array(_, _) => Err("cannot slice an array that is not addressable".into()),
            other => Err(format!("cannot slice {other}")),
        }
    }

    // ---- calls ------------------------------------------------------------

    fn call(&mut self, call: &ast::CallExpr) -> R<Val> {
        let objs = self.objs;
        match strip_parens(&call.func) {
            Expr::Ident(key) => {
                let name = objs.idents[*key].name.as_str();
                if self.lookup(name).is_some() {
                    return Err(format!(
                        "{name}: calling a function value is not supported yet"
                    ));
                }
                if let Some(sig) = self.funcs.get(name).cloned() {
                    return self.user_call(name, &sig, None, call);
                }
                match name {
                    "len" | "cap" => self.len_cap(name, call),
                    "append" => self.append(call),
                    "make" => self.make(call),
                    "new" => {
                        let [arg] = call.args.as_slice() else {
                            return Err("new takes exactly one type".into());
                        };
                        let ty = self.resolve_type(arg)?;
                        let ptr = self.heap_alloc(&ty);
                        Ok(Val::new(Ty::Ptr(Box::new(ty)), ptr))
                    }
                    "println" => self.print(&call.args, true),
                    "print" => self.print(&call.args, false),
                    "panic" => self.panic(call),
                    _ => match self.resolve_type(&call.func) {
                        Ok(ty) => self.conversion(&ty, call),
                        Err(_) => Err(format!("undefined: {name}")),
                    },
                }
            }
            Expr::Selector(s) => self.method_call(s, call),
            Expr::Array(_) | Expr::Star(_) => {
                Err("conversions to slice and pointer types are not supported yet".into())
            }
            _ => Err("unsupported call target".into()),
        }
    }

    fn conversion(&mut self, ty: &Ty, call: &ast::CallExpr) -> R<Val> {
        let [arg] = call.args.as_slice() else {
            return Err(format!("conversion to {ty} takes exactly one argument"));
        };
        let val = self.expr(arg)?;
        self.convert(val, ty)
    }

    fn user_call(
        &mut self,
        name: &str,
        sig: &FuncSig,
        receiver: Option<Val>,
        call: &ast::CallExpr,
    ) -> R<Val> {
        if call.ellipsis.is_some() {
            return Err(format!("{name}: variadic calls are not supported yet"));
        }
        let mut args = Vec::new();
        let mut params = sig.params.iter();
        if let Some(recv) = receiver {
            params.next();
            args.push(format!("{} {}", recv.ty.ll(), recv.repr));
        }
        if params.len() != call.args.len() {
            return Err(format!(
                "{name}: expected {} arguments, found {}",
                params.len(),
                call.args.len()
            ));
        }
        for (param, arg) in params.zip(&call.args) {
            let val = self
                .expr_want(arg, param)
                .map_err(|e| format!("in call to {name}: {e}"))?;
            args.push(format!("{} {}", param.ll(), val.repr));
        }
        let callee = format!("{} @\"{}\"({})", sig.ret.ll(), sig.symbol, args.join(", "));
        if sig.ret == Ty::Void {
            self.emit(format!("call {callee}"));
            Ok(Val::void())
        } else {
            let name = self.ins(format!("call {callee}"));
            Ok(Val::new(sig.ret.clone(), name))
        }
    }

    fn method_call(&mut self, s: &ast::SelectorExpr, call: &ast::CallExpr) -> R<Val> {
        let objs = self.objs;
        let method = objs.idents[s.sel].name.as_str();
        self.check_package(&s.expr)?;

        let recv = self.loc(&s.expr)?;
        let recv_ty = match &recv {
            Loc::Place(_, ty) => ty,
            Loc::Value(val) => &val.ty,
        };
        let struct_name = match recv_ty {
            Ty::Struct(name) => name.clone(),
            Ty::Ptr(inner) => match inner.as_ref() {
                Ty::Struct(name) => name.clone(),
                other => return Err(format!("*{other} has no method {method}")),
            },
            other => return Err(format!("{other} has no method {method}")),
        };
        let key = format!("{struct_name}.{method}");
        let Some(sig) = self.funcs.get(&key).cloned() else {
            return Err(format!("{struct_name} has no method {method}"));
        };
        let struct_ty = Ty::Struct(struct_name);
        let ptr_ty = Ty::Ptr(Box::new(struct_ty.clone()));

        // Adjust the receiver to what the method declares, as Go does implicitly.
        let receiver = if sig.ptr_recv == Some(true) {
            match recv {
                Loc::Place(ptr, Ty::Struct(_)) => {
                    self.mark_escape(&s.expr);
                    Val::new(ptr_ty, ptr)
                }
                Loc::Place(ptr, ty) => self.load(&ptr, &ty),
                Loc::Value(val) if matches!(val.ty, Ty::Ptr(_)) => val,
                Loc::Value(val) => {
                    let slot = self.spill(&val);
                    Val::new(ptr_ty, slot)
                }
            }
        } else {
            let val = match recv {
                Loc::Place(ptr, ty) => self.load(&ptr, &ty),
                Loc::Value(val) => val,
            };
            if matches!(val.ty, Ty::Ptr(_)) {
                self.load(&val.repr, &struct_ty)
            } else {
                val
            }
        };
        self.user_call(&key, &sig, Some(receiver), call)
    }

    fn len_cap(&mut self, name: &str, call: &ast::CallExpr) -> R<Val> {
        let [arg] = call.args.as_slice() else {
            return Err(format!("{name} takes exactly one argument"));
        };
        let val = self.expr_m(arg)?;
        let array_len = |ty: &Ty| match ty {
            Ty::Array(_, n) => Some(*n),
            _ => None,
        };
        match &val.ty {
            Ty::Str if name == "len" => {
                let n = self.ins(format!("extractvalue %string {}, 1", val.repr));
                Ok(Val::new(INT, n))
            }
            Ty::Slice(_) => {
                let index = if name == "len" { 1 } else { 2 };
                let n = self.ins(format!("extractvalue %slice {}, {index}", val.repr));
                Ok(Val::new(INT, n))
            }
            Ty::Array(_, n) => Ok(Val::int(INT, *n as i128)),
            Ty::Ptr(inner) if array_len(inner).is_some() => {
                Ok(Val::int(INT, array_len(inner).unwrap() as i128))
            }
            other => Err(format!("invalid argument of type {other} for {name}")),
        }
    }

    fn make(&mut self, call: &ast::CallExpr) -> R<Val> {
        let Some(type_expr) = call.args.first() else {
            return Err("make needs a type".into());
        };
        let ty = self.resolve_type(type_expr)?;
        let Ty::Slice(elem) = &ty else {
            return Err(format!("make({ty}) is not supported yet"));
        };
        if call.args.len() < 2 || call.args.len() > 3 {
            return Err("make([]T, len[, cap]) takes a length and an optional capacity".into());
        }
        let len_val = self.expr(&call.args[1])?;
        let len = self.widen_i64(len_val)?;
        let cap = match call.args.get(2) {
            Some(e) => {
                let v = self.expr(e)?;
                self.widen_i64(v)?
            }
            None => len.clone(),
        };
        let negative = self.ins(format!("icmp slt i64 {cap}, 0"));
        let too_long = self.ins(format!("icmp ugt i64 {len}, {cap}"));
        let bad = self.ins(format!("or i1 {negative}, {too_long}"));
        self.panic_if(&bad, "makeslice: len out of range");
        let data = self.ins(format!("call ptr @calloc(i64 {cap}, i64 {})", sizeof(elem)));
        Ok(self.make_slice(ty.clone(), &data, &len, &cap))
    }

    fn append(&mut self, call: &ast::CallExpr) -> R<Val> {
        if call.ellipsis.is_some() {
            return Err("append(s, t...) is not supported yet".into());
        }
        let Some(first) = call.args.first() else {
            return Err("append needs a slice".into());
        };
        let mut slice = self.expr(first)?;
        let Ty::Slice(elem) = slice.ty.clone() else {
            return Err(format!(
                "first argument to append must be a slice, found {}",
                slice.ty
            ));
        };
        for arg in &call.args[1..] {
            let val = self.expr_want(arg, &elem)?;
            let slot = self.spill(&val);
            let name = self.ins(format!(
                "call %slice @goiaba.append(%slice {}, ptr {slot}, i64 {})",
                slice.repr,
                sizeof(&elem)
            ));
            slice = Val::new(slice.ty, name);
        }
        Ok(slice)
    }

    /// Appends the printf directive and operands that print `val` the way Go's `println` does.
    fn format_arg(&mut self, val: &Val, fmt: &mut String, ops: &mut Vec<String>) -> R<()> {
        match &val.ty {
            Ty::Int { signed, .. } => {
                let wide = self.convert(
                    val.clone(),
                    &Ty::Int {
                        bits: 64,
                        signed: *signed,
                    },
                )?;
                fmt.push_str(if *signed { "%lld" } else { "%llu" });
                ops.push(format!("i64 {}", wide.repr));
            }
            Ty::Float(_) => {
                let wide = self.convert(val.clone(), &Ty::Float(64))?;
                fmt.push_str("%+e");
                ops.push(format!("double {}", wide.repr));
            }
            Ty::Bool => {
                let text = self.ins(format!(
                    "select i1 {}, ptr @goiaba.true, ptr @goiaba.false",
                    val.repr
                ));
                fmt.push_str("%s");
                ops.push(format!("ptr {text}"));
            }
            Ty::Str => {
                let data = self.ins(format!("extractvalue %string {}, 0", val.repr));
                let len = self.ins(format!("extractvalue %string {}, 1", val.repr));
                let len32 = self.ins(format!("trunc i64 {len} to i32"));
                fmt.push_str("%.*s");
                ops.push(format!("i32 {len32}"));
                ops.push(format!("ptr {data}"));
            }
            Ty::Ptr(_) => {
                fmt.push_str("%p");
                ops.push(format!("ptr {}", val.repr));
            }
            other => return Err(format!("cannot print a value of type {other}")),
        }
        Ok(())
    }

    /// `print` and `println` write to standard error, like the Go builtins.
    fn print(&mut self, args: &[Expr], newline: bool) -> R<Val> {
        let mut fmt = String::new();
        let mut ops = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            if newline && i > 0 {
                fmt.push(' ');
            }
            let val = self.expr_m(arg)?;
            self.format_arg(&val, &mut fmt, &mut ops)?;
        }
        if newline {
            fmt.push('\n');
        }
        self.dprintf(&fmt, &ops);
        Ok(Val::void())
    }

    fn dprintf(&mut self, fmt: &str, ops: &[String]) {
        let fmt = self.cstr(fmt);
        let mut call = format!("call i32 (i32, ptr, ...) @dprintf(i32 2, ptr {fmt}");
        for op in ops {
            call.push_str(", ");
            call.push_str(op);
        }
        call.push(')');
        self.ins(call);
    }

    fn panic(&mut self, call: &ast::CallExpr) -> R<Val> {
        let [arg] = call.args.as_slice() else {
            return Err("panic takes exactly one argument".into());
        };
        let val = self.expr_m(arg)?;
        let mut fmt = String::from("panic: ");
        let mut ops = Vec::new();
        self.format_arg(&val, &mut fmt, &mut ops)?;
        fmt.push_str("\n\n");
        self.dprintf(&fmt, &ops);
        self.emit("call void @exit(i32 2)".into());
        self.emit("unreachable".into());
        self.f.terminated = true;
        Ok(Val::void())
    }

    // ---- statements -------------------------------------------------------

    fn block(&mut self, stmts: &[Stmt]) -> R<()> {
        self.push_scope();
        let result = stmts.iter().try_for_each(|s| self.stmt(s));
        self.pop_scope();
        result
    }

    fn stmt(&mut self, s: &Stmt) -> R<()> {
        let objs = self.objs;
        match s {
            Stmt::Empty(_) => Ok(()),
            Stmt::Block(b) => self.block(&b.list),
            Stmt::Expr(e) => {
                if !matches!(strip_parens(e), Expr::Call(_)) {
                    return Err("expression statement must be a call".into());
                }
                self.expr(e).map(|_| ())
            }
            Stmt::Decl(decl) => self.local_decl(decl),
            Stmt::Assign(key) => self.assign(&objs.a_stmts[*key]),
            Stmt::IncDec(inc) => {
                let op = if matches!(inc.token, Token::INC) {
                    Op::Add
                } else {
                    Op::Sub
                };
                let Loc::Place(ptr, ty) = self.loc(&inc.expr)? else {
                    return Err("cannot increment this expression".into());
                };
                let cur = self.load(&ptr, &ty);
                let next = self.binary(op, cur, Val::int(Ty::UntypedInt, 1))?;
                self.store(&next, &ptr);
                Ok(())
            }
            Stmt::Return(ret) => self.ret(ret),
            Stmt::If(stmt) => self.if_stmt(stmt),
            Stmt::For(stmt) => self.for_stmt(stmt),
            Stmt::Range(stmt) => self.range_stmt(stmt),
            Stmt::Switch(stmt) => self.switch_stmt(stmt),
            Stmt::Branch(branch) => self.branch(branch),
            Stmt::Labeled(_) => Err("labeled statements are not supported yet".into()),
            Stmt::Go(_) => Err("goroutines are not supported yet".into()),
            Stmt::Defer(_) => Err("defer is not supported yet".into()),
            Stmt::Send(_) | Stmt::Select(_) | Stmt::Comm(_) => {
                Err("channels are not supported yet".into())
            }
            Stmt::TypeSwitch(_) => Err("type switches are not supported yet".into()),
            Stmt::Case(_) => Err("case clause outside of a switch".into()),
            Stmt::Bad(_) => Err("invalid statement".into()),
        }
    }

    fn local_decl(&mut self, decl: &Decl) -> R<()> {
        let objs = self.objs;
        let Decl::Gen(g) = decl else {
            return Err("unsupported local declaration".into());
        };
        for key in &g.specs {
            match &objs.specs[*key] {
                Spec::Type(ts) => {
                    let name = objs.idents[ts.name].name.clone();
                    if let Expr::Struct(st) = &ts.typ {
                        self.declare_struct_name(&name);
                        let fields = self.struct_fields(st)?;
                        self.structs.insert(name, fields);
                    } else {
                        let ty = self.resolve_type(&ts.typ)?;
                        self.aliases.insert(name, ty);
                    }
                }
                Spec::Value(vs) if matches!(g.token, Token::CONST) => {
                    for (name, val) in self.const_spec(vs)? {
                        self.bind(&name, Binding::Const(val));
                    }
                }
                Spec::Value(vs) => self.var_spec(vs)?,
                Spec::Import(_) => return Err("import inside a function".into()),
            }
        }
        Ok(())
    }

    fn var_spec(&mut self, vs: &ast::ValueSpec) -> R<()> {
        let objs = self.objs;
        let want = match &vs.typ {
            Some(t) => Some(self.resolve_type(t)?),
            None => None,
        };
        let names: Vec<&str> = vs
            .names
            .iter()
            .map(|n| objs.idents[*n].name.as_str())
            .collect();

        if vs.values.is_empty() {
            let ty = want.ok_or("var declaration needs a type or a value")?;
            for name in names {
                self.declare(name, ty.clone(), None);
            }
            return Ok(());
        }
        let mut vals = self.eval_rhs(&vs.values, names.len(), want.as_ref())?;
        for (name, val) in names.into_iter().zip(vals.drain(..)) {
            let val = match &want {
                Some(ty) => self.coerce(val, ty)?,
                None => self.materialize(val)?,
            };
            self.declare(name, val.ty.clone(), Some(val));
        }
        Ok(())
    }

    /// Evaluates the right side of an assignment to exactly `n` values.
    /// A single call returning a tuple is spread over the targets.
    fn eval_rhs(&mut self, rhs: &[Expr], n: usize, want: Option<&Ty>) -> R<Vec<Val>> {
        if rhs.len() == n {
            return rhs
                .iter()
                .map(|e| match want {
                    Some(ty) => self.expr_want(e, ty),
                    None => self.expr(e),
                })
                .collect();
        }
        if rhs.len() == 1 {
            let val = self.expr(&rhs[0])?;
            if let Ty::Tuple(items) = &val.ty
                && items.len() == n
            {
                let ll = val.ty.ll();
                return Ok(items
                    .iter()
                    .enumerate()
                    .map(|(i, ty)| {
                        let name = self.ins(format!("extractvalue {ll} {}, {i}", val.repr));
                        Val::new(ty.clone(), name)
                    })
                    .collect());
            }
        }
        Err(format!(
            "assignment mismatch: {n} variables but {} values",
            rhs.len()
        ))
    }

    fn assign(&mut self, a: &ast::AssignStmt) -> R<()> {
        let objs = self.objs;
        if let Some(op) = Op::from_assign(&a.token) {
            let ([lhs], [rhs]) = (a.lhs.as_slice(), a.rhs.as_slice()) else {
                return Err("compound assignment takes one operand on each side".into());
            };
            let Loc::Place(ptr, ty) = self.loc(lhs)? else {
                return Err("cannot assign to this expression".into());
            };
            let cur = self.load(&ptr, &ty);
            let right = self.expr(rhs)?;
            let result = self.binary(op, cur, right)?;
            let result = self.coerce(result, &ty)?;
            self.store(&result, &ptr);
            return Ok(());
        }

        let define = match a.token {
            Token::DEFINE => true,
            Token::ASSIGN => false,
            _ => return Err(format!("unsupported assignment operator {:?}", a.token)),
        };

        // A single typed target lets an untyped literal on the right pick up its type.
        if !define && a.lhs.len() == 1 && a.rhs.len() == 1 {
            if let Expr::Ident(key) = strip_parens(&a.lhs[0])
                && objs.idents[*key].name == "_"
            {
                return self.expr(&a.rhs[0]).map(|_| ());
            }
            let Loc::Place(ptr, ty) = self.loc(&a.lhs[0])? else {
                return Err("cannot assign to this expression".into());
            };
            let val = self.expr_want(&a.rhs[0], &ty)?;
            self.store(&val, &ptr);
            return Ok(());
        }

        let vals = self.eval_rhs(&a.rhs, a.lhs.len(), None)?;
        for (lhs, val) in a.lhs.iter().zip(vals) {
            let name = match strip_parens(lhs) {
                Expr::Ident(key) => Some(objs.idents[*key].name.as_str()),
                _ => None,
            };
            if name == Some("_") {
                continue;
            }
            let is_new = define
                && name.is_some_and(|n| {
                    !self
                        .f
                        .scopes
                        .last()
                        .is_some_and(|scope| scope.contains_key(n))
                });
            if is_new {
                let val = self.materialize(val)?;
                self.declare(name.unwrap(), val.ty.clone(), Some(val));
            } else {
                let Loc::Place(ptr, ty) = self.loc(lhs)? else {
                    return Err("cannot assign to this expression".into());
                };
                let val = self.coerce(val, &ty)?;
                self.store(&val, &ptr);
            }
        }
        Ok(())
    }

    fn ret(&mut self, ret: &ast::ReturnStmt) -> R<()> {
        let want = self.f.ret.clone();
        match (&want, ret.results.len()) {
            (Ty::Void, 0) => self.emit("ret void".into()),
            (Ty::Void, _) => return Err("too many return values".into()),
            (_, 0) => return Err("missing return value".into()),
            (Ty::Tuple(items), n) if n == items.len() => {
                let ll = want.ll();
                let mut cur = "poison".to_string();
                for (i, (e, ty)) in ret.results.iter().zip(items).enumerate() {
                    let val = self.expr_want(e, ty)?;
                    cur = self.ins(format!(
                        "insertvalue {ll} {cur}, {} {}, {i}",
                        ty.ll(),
                        val.repr
                    ));
                }
                self.emit(format!("ret {ll} {cur}"));
            }
            (_, 1) => {
                let val = self.expr_want(&ret.results[0], &want)?;
                self.emit(format!("ret {} {}", want.ll(), val.repr));
            }
            (_, n) => return Err(format!("wrong number of return values: {n}")),
        }
        self.f.terminated = true;
        Ok(())
    }

    fn if_stmt(&mut self, stmt: &ast::IfStmt) -> R<()> {
        self.push_scope();
        let result = (|| {
            if let Some(init) = &stmt.init {
                self.stmt(init)?;
            }
            let cond = self.expr_want(&stmt.cond, &Ty::Bool)?;
            let then = self.new_label("if.then");
            let end = self.new_label("if.end");
            let els = match &stmt.els {
                Some(_) => self.new_label("if.else"),
                None => end.clone(),
            };
            self.cond_br(&cond.repr, &then, &els);
            self.place_label(&then);
            self.block(&stmt.body.list)?;
            self.br(&end);
            if let Some(else_stmt) = &stmt.els {
                self.place_label(&els);
                self.stmt(else_stmt)?;
                self.br(&end);
            }
            self.place_label(&end);
            Ok(())
        })();
        self.pop_scope();
        result
    }

    fn for_stmt(&mut self, stmt: &ast::ForStmt) -> R<()> {
        self.push_scope();
        let result = (|| {
            if let Some(init) = &stmt.init {
                self.stmt(init)?;
            }
            let cond = self.new_label("for.cond");
            let body = self.new_label("for.body");
            let post = self.new_label("for.post");
            let end = self.new_label("for.end");
            self.place_label(&cond);
            if let Some(e) = &stmt.cond {
                let val = self.expr_want(e, &Ty::Bool)?;
                self.cond_br(&val.repr, &body, &end);
            } else {
                self.br(&body);
            }
            self.place_label(&body);
            self.f.targets.push((end.clone(), Some(post.clone())));
            let body_result = self.block(&stmt.body.list);
            self.f.targets.pop();
            body_result?;
            self.place_label(&post);
            if let Some(post_stmt) = &stmt.post {
                self.stmt(post_stmt)?;
            }
            self.br(&cond);
            self.place_label(&end);
            Ok(())
        })();
        self.pop_scope();
        result
    }

    fn range_stmt(&mut self, stmt: &ast::RangeStmt) -> R<()> {
        self.push_scope();
        let result = (|| {
            let subject = self.expr_m(&stmt.expr)?;
            // `elems` is the first element's address and the element type.
            let (len, elems) = match &subject.ty {
                Ty::Int { .. } => (self.widen_i64(subject.clone())?, None),
                Ty::Slice(elem) => {
                    let data = self.ins(format!("extractvalue %slice {}, 0", subject.repr));
                    let len = self.ins(format!("extractvalue %slice {}, 1", subject.repr));
                    (len, Some((data, elem.as_ref().clone())))
                }
                Ty::Array(elem, n) => {
                    let slot = self.spill(&subject);
                    (n.to_string(), Some((slot, elem.as_ref().clone())))
                }
                other => return Err(format!("cannot range over {other} yet")),
            };
            let define = matches!(stmt.token, Token::DEFINE);
            let counter = self.alloca(&INT);
            self.store(&Val::int(INT, 0), &counter);

            let cond = self.new_label("range.cond");
            let body = self.new_label("range.body");
            let post = self.new_label("range.post");
            let end = self.new_label("range.end");
            self.place_label(&cond);
            let i = self.load(&counter, &INT);
            let more = self.ins(format!("icmp slt i64 {}, {len}", i.repr));
            self.cond_br(&more, &body, &end);

            self.place_label(&body);
            self.push_scope();
            let body_result = (|| {
                if let Some(key) = &stmt.key {
                    self.range_bind(key, i.clone(), define)?;
                }
                if let Some(value) = &stmt.val {
                    let Some((data, elem)) = &elems else {
                        return Err("range over an integer yields only one value".to_string());
                    };
                    let slot = self.ins(format!(
                        "getelementptr inbounds {}, ptr {data}, i64 {}",
                        elem.ll(),
                        i.repr
                    ));
                    let item = self.load(&slot, elem);
                    self.range_bind(value, item, define)?;
                }
                self.f.targets.push((end.clone(), Some(post.clone())));
                let r = self.block(&stmt.body.list);
                self.f.targets.pop();
                r
            })();
            self.pop_scope();
            body_result?;

            self.place_label(&post);
            let cur = self.load(&counter, &INT);
            let next = self.ins(format!("add i64 {}, 1", cur.repr));
            self.store(&Val::new(INT, next), &counter);
            self.br(&cond);
            self.place_label(&end);
            Ok(())
        })();
        self.pop_scope();
        result
    }

    fn range_bind(&mut self, target: &Expr, val: Val, define: bool) -> R<()> {
        let objs = self.objs;
        if let Expr::Ident(key) = strip_parens(target) {
            let name = objs.idents[*key].name.as_str();
            if name == "_" {
                return Ok(());
            }
            if define {
                self.declare(name, val.ty.clone(), Some(val));
                return Ok(());
            }
        }
        let Loc::Place(ptr, ty) = self.loc(target)? else {
            return Err("cannot assign to this range variable".into());
        };
        let val = self.coerce(val, &ty)?;
        self.store(&val, &ptr);
        Ok(())
    }

    fn switch_stmt(&mut self, stmt: &ast::SwitchStmt) -> R<()> {
        self.push_scope();
        let result = (|| {
            if let Some(init) = &stmt.init {
                self.stmt(init)?;
            }
            let tag = match &stmt.tag {
                Some(e) => Some(self.expr_m(e)?),
                None => None,
            };
            let end = self.new_label("switch.end");
            let mut clauses = Vec::new();
            for s in &stmt.body.list {
                let Stmt::Case(clause) = s else {
                    return Err("unexpected statement in switch body".to_string());
                };
                clauses.push((clause, self.new_label("switch.case")));
            }

            // Tests run in source order; `default` is taken only when none match.
            let mut default = None;
            for (clause, label) in &clauses {
                let Some(exprs) = clause.list.as_ref().filter(|l| !l.is_empty()) else {
                    default = Some(label.clone());
                    continue;
                };
                for e in exprs {
                    let hit = match &tag {
                        Some(tag) => {
                            let val = self.expr(e)?;
                            self.binary(Op::Eq, tag.clone(), val)?
                        }
                        None => self.expr_want(e, &Ty::Bool)?,
                    };
                    let next = self.new_label("switch.next");
                    self.cond_br(&hit.repr, label, &next);
                    self.place_label(&next);
                }
            }
            self.br(default.as_deref().unwrap_or(&end));

            for (clause, label) in &clauses {
                self.place_label(label);
                self.f.targets.push((end.clone(), None));
                let r = self.block(&clause.body);
                self.f.targets.pop();
                r?;
                self.br(&end);
            }
            self.place_label(&end);
            Ok(())
        })();
        self.pop_scope();
        result
    }

    fn branch(&mut self, branch: &ast::BranchStmt) -> R<()> {
        if branch.label.is_some() {
            return Err("labeled break and continue are not supported yet".into());
        }
        let target = match branch.token {
            Token::BREAK => self.f.targets.last().map(|(brk, _)| brk.clone()),
            Token::CONTINUE => self
                .f
                .targets
                .iter()
                .rev()
                .find_map(|(_, cont)| cont.clone()),
            Token::FALLTHROUGH => return Err("fallthrough is not supported yet".into()),
            _ => return Err("goto is not supported yet".into()),
        };
        let Some(target) = target else {
            return Err("break or continue outside of a loop".into());
        };
        self.br(&target);
        Ok(())
    }
}

/// Constant-folds an operation on two untyped numeric constants.
fn fold(op: Op, left: &Val, right: &Val) -> R<Val> {
    let ty = if left.ty == Ty::UntypedFloat || right.ty == Ty::UntypedFloat {
        Ty::UntypedFloat
    } else if left.ty == Ty::UntypedRune || right.ty == Ty::UntypedRune {
        Ty::UntypedRune
    } else {
        Ty::UntypedInt
    };
    let overflow = || "constant overflow".to_string();
    match (left.k, right.k) {
        (Some(K::Int(a)), Some(K::Int(b))) => {
            let v = match op {
                Op::Add => a.checked_add(b).ok_or_else(overflow)?,
                Op::Sub => a.checked_sub(b).ok_or_else(overflow)?,
                Op::Mul => a.checked_mul(b).ok_or_else(overflow)?,
                Op::Div | Op::Rem if b == 0 => return Err("division by zero".into()),
                Op::Div => a / b,
                Op::Rem => a % b,
                Op::And => a & b,
                Op::Or => a | b,
                Op::Xor => a ^ b,
                Op::AndNot => a & !b,
                Op::Eq => return Ok(Val::bool(a == b)),
                Op::Ne => return Ok(Val::bool(a != b)),
                Op::Lt => return Ok(Val::bool(a < b)),
                Op::Le => return Ok(Val::bool(a <= b)),
                Op::Gt => return Ok(Val::bool(a > b)),
                Op::Ge => return Ok(Val::bool(a >= b)),
                Op::Shl | Op::Shr => return Err("internal: shift reached fold".into()),
            };
            Ok(Val::int(ty, v))
        }
        (Some(a), Some(b)) => {
            let (a, b) = (a.as_f64(), b.as_f64());
            let v = match op {
                Op::Add => a + b,
                Op::Sub => a - b,
                Op::Mul => a * b,
                Op::Div if b == 0.0 => return Err("division by zero".into()),
                Op::Div => a / b,
                Op::Eq => return Ok(Val::bool(a == b)),
                Op::Ne => return Ok(Val::bool(a != b)),
                Op::Lt => return Ok(Val::bool(a < b)),
                Op::Le => return Ok(Val::bool(a <= b)),
                Op::Gt => return Ok(Val::bool(a > b)),
                Op::Ge => return Ok(Val::bool(a >= b)),
                _ => {
                    return Err(format!(
                        "operator {} is not defined on untyped float",
                        op.symbol()
                    ));
                }
            };
            Ok(Val::float(Ty::UntypedFloat, v))
        }
        _ => Err("internal: untyped value without a constant".into()),
    }
}
