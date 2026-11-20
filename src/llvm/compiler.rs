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
use super::stdlib;
use crate::parser::ast::{self, Decl, Expr, Node, Spec, Stmt};
use crate::parser::objects::AstObjects;
use crate::parser::{Token, parse_str};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::Write as _;

type R<T> = Result<T, String>;

#[derive(Debug, Clone, PartialEq)]
enum Ty {
    Int {
        bits: u32,
        signed: bool,
    },
    Float(u32),
    Bool,
    Str,
    Struct(String),
    /// A defined type such as `type Celsius float64`: its name and underlying type.
    Named(String, Box<Ty>),
    Ptr(Box<Ty>),
    Array(Box<Ty>, u64),
    Slice(Box<Ty>),
    /// A map: key and value types. The value is a pointer to a runtime table.
    Map(Box<Ty>, Box<Ty>),
    Tuple(Vec<Ty>),
    /// A function value: a code pointer plus a context pointer.
    Func(Box<FuncTy>),
    /// An interface: its methods sorted by name. Empty means `any`.
    Iface(Vec<(String, FuncTy)>),
    Void,
    UntypedInt,
    UntypedRune,
    UntypedFloat,
    Nil,
}

/// The signature of a function value.
#[derive(Debug, Clone, PartialEq)]
struct FuncTy {
    params: Vec<Ty>,
    ret: Ty,
    variadic: bool,
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
            Ty::Struct(name) if name.contains('.') => format!("%\"{name}\""),
            Ty::Struct(name) => format!("%\"main.{name}\""),
            Ty::Named(_, inner) => inner.ll(),
            Ty::Ptr(_) | Ty::Map(..) | Ty::Nil => "ptr".into(),
            Ty::Array(elem, n) => format!("[{n} x {}]", elem.ll()),
            Ty::Slice(_) => "%slice".into(),
            Ty::Func(_) => "%func".into(),
            Ty::Iface(_) => "%iface".into(),
            Ty::Tuple(items) => {
                let parts: Vec<String> = items.iter().map(Ty::ll).collect();
                format!("{{ {} }}", parts.join(", "))
            }
            Ty::Void => "void".into(),
            Ty::UntypedInt | Ty::UntypedRune => "i64".into(),
        }
    }

    /// The type with defined-type names stripped; operations dispatch on this.
    fn under(&self) -> &Ty {
        match self {
            Ty::Named(_, inner) => inner.under(),
            other => other,
        }
    }

    /// The name methods are declared on, if this type can have methods.
    fn type_name(&self) -> Option<&str> {
        match self {
            Ty::Struct(name) | Ty::Named(name, _) => Some(name),
            _ => None,
        }
    }

    /// The type's name as Go's runtime prints it, e.g. `main.Point` or `*main.Point`.
    fn go_name(&self) -> String {
        match self {
            Ty::Named(name, _) if name == "error" => name.clone(),
            Ty::Struct(name) | Ty::Named(name, _) if name.contains('.') => name.clone(),
            Ty::Struct(name) | Ty::Named(name, _) => format!("main.{name}"),
            Ty::Ptr(inner) => format!("*{}", inner.go_name()),
            Ty::Slice(elem) => format!("[]{}", elem.go_name()),
            Ty::Map(key, val) => format!("map[{}]{}", key.go_name(), val.go_name()),
            Ty::Array(elem, n) => format!("[{n}]{}", elem.go_name()),
            other => other.to_string(),
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
            Ty::Struct(name) | Ty::Named(name, _) => f.write_str(name),
            Ty::Ptr(inner) => write!(f, "*{inner}"),
            Ty::Array(elem, n) => write!(f, "[{n}]{elem}"),
            Ty::Slice(elem) => write!(f, "[]{elem}"),
            Ty::Map(key, val) => write!(f, "map[{key}]{val}"),
            Ty::Tuple(items) => {
                let parts: Vec<String> = items.iter().map(|t| t.to_string()).collect();
                write!(f, "({})", parts.join(", "))
            }
            Ty::Func(sig) => {
                let parts: Vec<String> = sig.params.iter().map(|t| t.to_string()).collect();
                write!(f, "func({})", parts.join(", "))?;
                match &sig.ret {
                    Ty::Void => Ok(()),
                    ret => write!(f, " {ret}"),
                }
            }
            Ty::Iface(methods) if methods.is_empty() => f.write_str("interface {}"),
            Ty::Iface(methods) => {
                let names: Vec<&str> = methods.iter().map(|(name, _)| name.as_str()).collect();
                write!(f, "interface {{ {} }}", names.join("; "))
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
    Var {
        ptr: String,
        ty: Ty,
        heap: bool,
        /// When set, `ptr` is a slot holding the variable's current address.
        /// Loop variables captured by closures get a fresh address per iteration.
        indirect: bool,
    },
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
    /// True when the last parameter is `...T`; it is passed as a slice.
    variadic: bool,
    /// True for a function declared without a body. It is implemented in C;
    /// strings, slices and interfaces are passed to it as separate scalars.
    extern_c: bool,
}

/// Parameters and results of a function type.
struct Signature {
    params: Vec<(String, Ty)>,
    /// Result names, empty when the results are unnamed.
    results: Vec<String>,
    ret: Ty,
    variadic: bool,
}

/// A variable a closure uses from an enclosing function.
struct Capture {
    /// The variable's address (or slot, when `indirect`) in the enclosing function.
    ptr: String,
    indirect: bool,
}

struct Shape {
    /// `name`, or `Type.name` for a method.
    key: String,
    /// Includes the receiver as the first parameter.
    sig: Signature,
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
    /// Named result variables, read by a bare `return`.
    named_results: Vec<String>,
    /// LLVM symbol of the function being lowered.
    symbol: String,
    /// Variables captured from enclosing functions, in context order.
    captures: Vec<Capture>,
    /// Definitions of the closures created inside this function.
    nested_defs: Vec<String>,
    closure_count: usize,
    /// True when the function has a defer frame and returns through `fn.exit`.
    uses_defer: bool,
    /// Set when a `defer` is met in a function lowered without a frame.
    wants_defer: bool,
    /// Where `return` stores results when the function uses defer.
    ret_slots: Vec<(String, Ty)>,
    /// The call captured for a `defer` or `go` statement instead of emitted.
    pending: Option<Pending>,
}

/// A fully evaluated call that has not been emitted.
struct Pending {
    ret: Ty,
    /// A function symbol such as `@"main.f"`, or `None` when the code
    /// pointer is the first operand.
    symbol: Option<String>,
    /// Operands as (LLVM type, value).
    args: Vec<(String, String)>,
}

impl Signature {
    fn func_ty(&self) -> FuncTy {
        FuncTy {
            params: self.params.iter().map(|(_, ty)| ty.clone()).collect(),
            ret: self.ret.clone(),
            variadic: self.variadic,
        }
    }
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
            named_results: Vec::new(),
            symbol: String::new(),
            captures: Vec::new(),
            nested_defs: Vec::new(),
            closure_count: 0,
            uses_defer: false,
            wants_defer: false,
            ret_slots: Vec::new(),
            pending: None,
        }
    }
}

/// One input file inside the concatenated source text.
struct SourceFile {
    name: String,
    start: usize,
}

/// One parsed package: the main program or an imported library.
struct Package {
    /// Empty for the main package, otherwise the package name.
    name: String,
    source: String,
    files: Vec<SourceFile>,
    objs: AstObjects,
    file: ast::File,
}

struct Compiler<'a> {
    objs: &'a AstObjects,
    source: &'a str,
    files: &'a [SourceFile],
    opts: Options,
    /// Name of the package being compiled; empty for main.
    pkg: String,
    /// Package-level variables whose initializers must run at start-up:
    /// the spec and the index of the name in it (`usize::MAX` for all names,
    /// when one call initializes several variables).
    pending_globals: Vec<(&'a ast::ValueSpec, usize)>,
    /// Package init functions, in the order they must run.
    inits: Vec<String>,
    has_main: bool,
    /// The integer type an untyped constant takes in `1 << n`, from context.
    shift_hint: Option<Ty>,
    /// True once an error has been given a source position.
    err_located: bool,
    structs: HashMap<String, Vec<(String, Ty)>>,
    struct_order: Vec<String>,
    aliases: HashMap<String, Ty>,
    funcs: HashMap<String, FuncSig>,
    globals: HashMap<String, Binding>,
    /// Imported packages of the current file set: local name to package name.
    packages: HashMap<String, String>,
    strings: Vec<Vec<u8>>,
    string_ids: HashMap<Vec<u8>, usize>,
    global_defs: Vec<String>,
    fn_defs: Vec<String>,
    /// Functions that already have a thunk for use as a function value.
    thunks: HashSet<String>,
    /// Method names declared on each type name.
    methods: HashMap<String, Vec<String>>,
    /// Type descriptor symbol for each type that has been put in an interface.
    rtypes: HashMap<String, String>,
    /// Equality function symbol (or `null`) per type.
    eq_fns: HashMap<String, String>,
    /// Interface descriptors and method tables already emitted.
    iface_globals: HashSet<String>,
    f: FnCtx,
    /// Enclosing functions while a closure body is being lowered.
    outer: Vec<FnCtx>,
}

/// Settings for one compilation.
#[derive(Debug, Clone, Default)]
pub struct Options {
    /// LLVM target triple to record in the module, e.g. `x86_64-unknown-linux-gnu`.
    pub target: Option<String>,
    /// LLVM data layout string that matches `target`.
    pub data_layout: Option<String>,
}

/// Compiles one Go source file to a textual LLVM IR module.
pub fn compile_str(go_source: &str) -> Result<String, Box<dyn std::error::Error>> {
    compile_sources(
        &[("main.go".to_string(), go_source.to_string())],
        &Options::default(),
    )
}

/// Compiles the files of the main package, given as `(name, text)` pairs,
/// together with the packages they import.
pub fn compile_sources(
    files: &[(String, String)],
    opts: &Options,
) -> Result<String, Box<dyn std::error::Error>> {
    let main = parse_package("", files)?;
    // Dependencies first, the main package last.
    let mut packages = Vec::new();
    load_imports(&main, &mut packages, &mut Vec::new())?;
    packages.push(main);

    let mut compiler = Compiler::new(&packages[0], opts.clone());
    for package in &packages {
        compiler.compile_package(package)?;
    }
    Ok(compiler.finish())
}

fn parse_package(name: &str, files: &[(String, String)]) -> Result<Package, String> {
    let mut source = String::new();
    let mut spans = Vec::new();
    for (file_name, text) in files {
        spans.push(SourceFile {
            name: file_name.clone(),
            start: source.len(),
        });
        source.push_str(text);
        source.push('\n');
    }
    let (objs, file) = parse_str(&source).map_err(|e| match files.first() {
        Some((file_name, _)) if !name.is_empty() => format!("{file_name}: {e}"),
        _ => e,
    })?;
    Ok(Package {
        name: name.to_string(),
        source,
        files: spans,
        objs,
        file,
    })
}

/// The import paths of a package with the local name each is bound to.
fn imports_of(package: &Package) -> Vec<(String, String)> {
    let objs = &package.objs;
    let mut out = Vec::new();
    for key in &package.file.imports {
        if let Spec::Import(import) = &objs.specs[*key] {
            let path = import
                .path
                .token
                .get_literal()
                .trim_matches('"')
                .to_string();
            let package_name = path.rsplit('/').next().unwrap_or(&path).to_string();
            let local = match import.name {
                Some(name) => objs.idents[name].name.clone(),
                None => package_name,
            };
            out.push((path, local));
        }
    }
    out
}

/// Parses every package `package` imports, directly or not, into `loaded`
/// in dependency order.
fn load_imports(
    package: &Package,
    loaded: &mut Vec<Package>,
    visiting: &mut Vec<String>,
) -> Result<(), String> {
    for (path, _) in imports_of(package) {
        let name = path.rsplit('/').next().unwrap_or(&path).to_string();
        if loaded.iter().any(|p| p.name == name) {
            continue;
        }
        if visiting.contains(&name) {
            return Err(format!("import cycle through package {path}"));
        }
        let Some(text) = stdlib::source(&path) else {
            return Err(format!(
                "package {path} is not available: goiaba ships a small subset of the standard library"
            ));
        };
        let dependency = parse_package(&name, &[(format!("{path}.go"), text.to_string())])?;
        visiting.push(name);
        load_imports(&dependency, loaded, visiting)?;
        visiting.pop();
        loaded.push(dependency);
    }
    Ok(())
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
        "any" => Some(Ty::Iface(Vec::new())),
        "error" => Some(Ty::Named(
            "error".into(),
            Box::new(Ty::Iface(vec![(
                "Error".into(),
                FuncTy {
                    params: Vec::new(),
                    ret: Ty::Str,
                    variadic: false,
                },
            )])),
        )),
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
    fn new(first: &'a Package, opts: Options) -> Self {
        Compiler {
            objs: &first.objs,
            source: &first.source,
            files: &first.files,
            opts,
            pkg: String::new(),
            pending_globals: Vec::new(),
            inits: Vec::new(),
            has_main: false,
            shift_hint: None,
            err_located: false,
            structs: HashMap::new(),
            struct_order: Vec::new(),
            aliases: HashMap::new(),
            funcs: HashMap::new(),
            globals: HashMap::new(),
            packages: HashMap::new(),
            strings: Vec::new(),
            string_ids: HashMap::new(),
            global_defs: Vec::new(),
            fn_defs: Vec::new(),
            thunks: HashSet::new(),
            methods: HashMap::new(),
            rtypes: HashMap::new(),
            eq_fns: HashMap::new(),
            iface_globals: HashSet::new(),
            f: FnCtx::new(Ty::Void, HashSet::new()),
            outer: Vec::new(),
        }
    }

    /// Formats a parser position as `file:line:column`.
    fn position(&self, pos: usize) -> String {
        let offset = pos.min(self.source.len());
        let file = self
            .files
            .iter()
            .rev()
            .find(|f| f.start <= offset)
            .or(self.files.first());
        let (name, start) = match file {
            Some(f) => (f.name.as_str(), f.start),
            None => ("main.go", 0),
        };
        let before = &self.source[start..offset];
        let line = before.matches('\n').count() + 1;
        let column = before.len() - before.rfind('\n').map_or(0, |i| i + 1) + 1;
        format!("{name}:{line}:{column}")
    }

    /// Prefixes an error with a source position, unless it already has one.
    fn locate(&mut self, pos: usize, error: String) -> String {
        if self.err_located {
            return error;
        }
        self.err_located = true;
        format!("{}: {error}", self.position(pos))
    }

    // ---- module level -----------------------------------------------------

    /// The key a package-level name is stored under: bare in main,
    /// `pkg.name` in any other package.
    fn q(&self, name: &str) -> String {
        if self.pkg.is_empty() {
            name.to_string()
        } else {
            format!("{}.{name}", self.pkg)
        }
    }

    /// The LLVM symbol for a package-level key.
    fn symbol_for(&self, key: &str) -> String {
        if self.pkg.is_empty() {
            format!("main.{key}")
        } else {
            key.to_string()
        }
    }

    fn compile_package(&mut self, package: &'a Package) -> R<()> {
        self.objs = &package.objs;
        self.source = &package.source;
        self.files = &package.files;
        self.pkg = package.name.clone();
        self.err_located = false;
        // Package-level declarations bind into the global scope, so no
        // function state may be left over from the previous package.
        self.f = FnCtx::new(Ty::Void, HashSet::new());
        self.packages.clear();
        for (path, local) in imports_of(package) {
            let name = path.rsplit('/').next().unwrap_or(&path).to_string();
            self.packages.insert(local, name);
        }
        let objs = self.objs;
        let file = &package.file;

        let mut type_specs = Vec::new();
        let mut value_decls = Vec::new();
        let mut func_decls = Vec::new();
        for decl in &file.decls {
            match decl {
                Decl::Gen(g) => match g.token {
                    Token::TYPE => {
                        for key in &g.specs {
                            if let Spec::Type(ts) = &objs.specs[*key] {
                                type_specs.push(ts);
                            }
                        }
                    }
                    Token::CONST | Token::VAR => value_decls.push(g),
                    _ => {}
                },
                Decl::Func(key) => func_decls.push(&objs.fdecls[*key]),
                Decl::Bad(_) => return Err("invalid declaration".into()),
            }
        }

        // Struct names first, so types can refer to each other in any order.
        for ts in &type_specs {
            if matches!(ts.typ, Expr::Struct(_)) {
                let name = self.q(&objs.idents[ts.name].name);
                self.declare_struct_name(&name);
            }
        }
        // Other defined types are resolved until no more of them make progress.
        let mut pending: Vec<_> = type_specs
            .iter()
            .copied()
            .filter(|ts| !matches!(ts.typ, Expr::Struct(_)))
            .collect();
        while !pending.is_empty() {
            let before = pending.len();
            let mut failed = Vec::new();
            let mut last_error = None;
            for ts in pending {
                match self.define_type(ts) {
                    Ok(()) => {}
                    Err(e) => {
                        last_error = Some((objs.idents[ts.name].pos, e));
                        failed.push(ts);
                    }
                }
            }
            if failed.len() == before {
                let (pos, e) = last_error.unwrap();
                return Err(self.locate(pos, e));
            }
            pending = failed;
        }
        for ts in &type_specs {
            if let Expr::Struct(st) = &ts.typ {
                let name = self.q(&objs.idents[ts.name].name);
                let fields = self.struct_fields(st);
                let fields = fields.map_err(|e| self.locate(objs.idents[ts.name].pos, e))?;
                self.structs.insert(name, fields);
            }
        }

        for g in value_decls {
            let result = if matches!(g.token, Token::CONST) {
                self.const_group(&g.specs)
            } else {
                g.specs.iter().try_for_each(|key| match &objs.specs[*key] {
                    Spec::Value(vs) => self.global_var_spec(vs),
                    _ => Ok(()),
                })
            };
            result.map_err(|e| self.locate(g.token_pos, e))?;
        }

        // Signatures first, so bodies and initializers can call any function.
        let mut keys = Vec::new();
        let mut init_funcs = Vec::new();
        for fd in &func_decls {
            let pos = objs.idents[fd.name].pos;
            let shape = self.func_shape(fd);
            let mut shape = shape.map_err(|e| self.locate(pos, e))?;
            let name = objs.idents[fd.name].name.as_str();
            let is_method = shape.ptr_recv.is_some();
            let extern_c = fd.body.is_none();
            if name == "init" && !is_method {
                // A package may declare several init functions.
                shape.key = self.q(&format!("init${}", init_funcs.len()));
            }
            let symbol = if extern_c {
                self.declare_extern(name, &shape.sig)
                    .map_err(|e| self.locate(pos, e))?
            } else if is_method || !self.pkg.is_empty() {
                self.symbol_for(&shape.key)
            } else if let Some(exported) = export_name(self.source, name) {
                exported
            } else {
                self.has_main |= name == "main";
                self.symbol_for(&shape.key)
            };
            if name == "init" && !is_method {
                init_funcs.push(symbol.clone());
            }
            if is_method && let Some((type_name, method)) = shape.key.rsplit_once('.') {
                let list = self.methods.entry(type_name.to_string()).or_default();
                list.push(method.to_string());
            }
            if self.funcs.contains_key(&shape.key) {
                return Err(self.locate(pos, format!("{} redeclared", shape.key)));
            }
            keys.push(shape.key.clone());
            self.funcs.insert(
                shape.key,
                FuncSig {
                    symbol,
                    params: shape.sig.params.into_iter().map(|(_, ty)| ty).collect(),
                    ret: shape.sig.ret,
                    ptr_recv: shape.ptr_recv,
                    variadic: shape.sig.variadic,
                    extern_c,
                },
            );
        }

        self.lower_init(&init_funcs)?;

        for (fd, key) in func_decls.iter().zip(&keys) {
            self.compile_func(fd, key)?;
        }
        Ok(())
    }

    /// Declares a body-less function as an external C symbol and returns the
    /// symbol. Aggregates are split: a string is passed as (pointer, length),
    /// a slice as (pointer, length, capacity), an interface as two pointers.
    fn declare_extern(&mut self, name: &str, sig: &Signature) -> R<String> {
        let symbol = if self.pkg.is_empty() {
            name.to_string()
        } else {
            format!("goiaba_{}_{name}", self.pkg)
        };
        let mut params = Vec::new();
        for (_, ty) in &sig.params {
            match ty.under() {
                Ty::Str => params.push("ptr, i64".to_string()),
                Ty::Slice(_) => params.push("ptr, i64, i64".to_string()),
                Ty::Iface(_) => params.push("ptr, ptr".to_string()),
                Ty::Int { .. } | Ty::Float(_) | Ty::Bool | Ty::Ptr(_) | Ty::Map(..) => {
                    params.push(ty.ll())
                }
                _ => {
                    return Err(format!(
                        "{name}: a function without a body cannot take a {ty} parameter"
                    ));
                }
            }
        }
        if !matches!(
            sig.ret.under(),
            Ty::Void | Ty::Int { .. } | Ty::Float(_) | Ty::Bool | Ty::Ptr(_) | Ty::Map(..)
        ) {
            return Err(format!(
                "{name}: a function without a body cannot return {}",
                sig.ret
            ));
        }
        if sig.variadic {
            return Err(format!(
                "{name}: a function without a body cannot be variadic"
            ));
        }
        self.global_defs.push(format!(
            "declare {} @\"{symbol}\"({})",
            sig.ret.ll(),
            params.join(", ")
        ));
        Ok(symbol)
    }

    /// Lowers the package's init function: it runs the initializers of the
    /// package-level variables that are not constants, then the user's
    /// `init` functions.
    fn lower_init(&mut self, init_funcs: &[String]) -> R<()> {
        let objs = self.objs;
        let pending = std::mem::take(&mut self.pending_globals);
        if pending.is_empty() && init_funcs.is_empty() {
            return Ok(());
        }
        let symbol = self.symbol_for(&self.q(".init"));
        self.f = FnCtx::new(Ty::Void, HashSet::new());
        self.f.symbol = symbol.clone();
        for (vs, index) in pending {
            let pos = objs.idents[vs.names[0]].pos;
            let result = (|| {
                let want = match &vs.typ {
                    Some(t) => Some(self.resolve_type(t)?),
                    None => None,
                };
                let (names, vals): (Vec<_>, Vec<_>) = if index == usize::MAX {
                    let vals = self.eval_rhs(&vs.values, vs.names.len(), want.as_ref())?;
                    (vs.names.iter().collect(), vals)
                } else {
                    let val = match &want {
                        Some(ty) => self.expr_want(&vs.values[index], ty)?,
                        None => self.expr(&vs.values[index])?,
                    };
                    (vec![&vs.names[index]], vec![val])
                };
                for (name, val) in names.into_iter().zip(vals) {
                    let val = match &want {
                        Some(ty) => self.coerce(val, ty)?,
                        None => self.materialize(val)?,
                    };
                    let name = objs.idents[*name].name.as_str();
                    if name == "_" {
                        continue;
                    }
                    let ptr = self.define_global(name, &val.ty, "zeroinitializer");
                    self.store(&val, &ptr);
                }
                Ok(())
            })();
            result.map_err(|e: String| self.locate(pos, e))?;
        }
        for init in init_funcs {
            self.emit(format!("call void @\"{init}\"()"));
        }
        self.emit("ret void".into());

        let mut def = format!("define internal void @\"{symbol}\"() {{\nentry:\n");
        for line in self.f.allocas.iter().chain(&self.f.body) {
            def.push_str(line);
            def.push('\n');
        }
        def.push_str("}\n");
        let nested = std::mem::take(&mut self.f.nested_defs);
        self.fn_defs.extend(nested);
        self.fn_defs.push(def);
        self.inits.push(symbol);
        Ok(())
    }

    /// Emits a package-level variable and binds its name.
    fn define_global(&mut self, name: &str, ty: &Ty, init: &str) -> String {
        let key = self.q(name);
        let symbol = format!("@\"{}\"", self.symbol_for(&key));
        self.global_defs
            .push(format!("{symbol} = internal global {} {init}", ty.ll()));
        self.globals.insert(
            key,
            Binding::Var {
                ptr: symbol.clone(),
                ty: ty.clone(),
                heap: true,
                indirect: false,
            },
        );
        symbol
    }

    /// Assembles the module once every package has been compiled.
    fn finish(&mut self) -> String {
        let mut out = String::new();
        out.push_str("; Generated by goiaba\nsource_filename = \"main.go\"\n");
        if let Some(layout) = &self.opts.data_layout {
            let _ = writeln!(out, "target datalayout = \"{layout}\"");
        }
        if let Some(target) = &self.opts.target {
            let _ = writeln!(out, "target triple = \"{target}\"");
        }
        out.push_str("\n%string = type { ptr, i64 }\n%slice = type { ptr, i64, i64 }\n");
        out.push_str("%func = type { ptr, ptr }\n%iface = type { ptr, ptr }\n");
        out.push_str("%rtype = type { i32, i32, i64, ptr, i64, ptr, i64, ptr, ptr, i64, ptr }\n");
        out.push_str("%rmethod = type { ptr, i64, ptr }\n%rfield = type { ptr, i64, ptr, i64 }\n");
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
        out.push_str(prelude());
        for def in &self.fn_defs {
            out.push('\n');
            out.push_str(def);
        }
        // Package initializers run in dependency order. A library without a
        // main function can call this before using exported functions.
        out.push_str("\ndefine void @goiaba_run_inits() {\nentry:\n");
        for init in &self.inits {
            let _ = writeln!(out, "  call void @\"{init}\"()");
        }
        out.push_str("  ret void\n}\n");
        if self.has_main {
            out.push_str(
                "\ndefine i32 @main(i32 %argc, ptr %argv) {\nentry:\n  call void @goiaba_init(i32 %argc, ptr %argv)\n  call void @goiaba_run_inits()\n  call void @\"main.main\"()\n  call void @goiaba_exit(i32 0)\n  unreachable\n}\n",
            );
        }
        out
    }

    /// Registers `type Name T`. An alias (`type Name = T`) keeps T's identity.
    fn define_type(&mut self, ts: &ast::TypeSpec) -> R<()> {
        let name = self.q(&self.objs.idents[ts.name].name);
        let ty = self.resolve_type(&ts.typ)?;
        let ty = if ts.assign > 0 {
            ty
        } else {
            Ty::Named(name.clone(), Box::new(ty))
        };
        self.aliases.insert(name, ty);
        Ok(())
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
                let key = self.q(name);
                if self.structs.contains_key(&key) {
                    Ok(Ty::Struct(key))
                } else if let Some(ty) = self.aliases.get(&key) {
                    Ok(ty.clone())
                } else if let Some(ty) = builtin_type(name) {
                    Ok(ty)
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
            Expr::Map(m) => {
                let key = self.resolve_type(&m.key)?;
                let val = self.resolve_type(&m.val)?;
                if !matches!(
                    key.under(),
                    Ty::Int { .. } | Ty::Bool | Ty::Float(_) | Ty::Str | Ty::Ptr(_)
                ) {
                    return Err(format!("map key type {key} is not supported yet"));
                }
                Ok(Ty::Map(Box::new(key), Box::new(val)))
            }
            Expr::Chan(_) => Err("channel types are not supported yet".into()),
            Expr::Interface(it) => {
                let mut methods = Vec::new();
                for field_key in &it.methods.list {
                    let field = &objs.fields[*field_key];
                    if field.names.is_empty() {
                        // An embedded interface contributes its methods.
                        let embedded = self.resolve_type(&field.typ)?;
                        let Ty::Iface(inner) = embedded.under() else {
                            return Err(format!("{embedded} is not an interface"));
                        };
                        methods.extend(inner.iter().cloned());
                        continue;
                    }
                    let Expr::Func(key) = &field.typ else {
                        return Err("invalid interface method".into());
                    };
                    let sig = self.signature(&objs.ftypes[*key])?.func_ty();
                    for name in &field.names {
                        methods.push((objs.idents[*name].name.clone(), sig.clone()));
                    }
                }
                methods.sort_by(|a, b| a.0.cmp(&b.0));
                methods.dedup_by(|a, b| a.0 == b.0);
                Ok(Ty::Iface(methods))
            }
            Expr::Func(key) => {
                let sig = self.signature(&objs.ftypes[*key])?;
                Ok(Ty::Func(Box::new(sig.func_ty())))
            }
            Expr::Selector(s) => {
                let Some(package) = self.package_of(&s.expr) else {
                    return Err("expression is not a type".into());
                };
                let key = format!("{package}.{}", objs.idents[s.sel].name);
                if self.structs.contains_key(&key) {
                    Ok(Ty::Struct(key))
                } else if let Some(ty) = self.aliases.get(&key) {
                    Ok(ty.clone())
                } else {
                    Err(format!("undefined type: {key}"))
                }
            }
            _ => Err("expression is not a type".into()),
        }
    }

    /// Evaluates a `const (...)` group and binds its names. A spec without
    /// values repeats the previous one, and `iota` is the spec's index.
    fn const_group(&mut self, specs: &[crate::parser::SpecKey]) -> R<()> {
        let objs = self.objs;
        let mut previous: Option<&ast::ValueSpec> = None;
        for (index, key) in specs.iter().enumerate() {
            let Spec::Value(vs) = &objs.specs[*key] else {
                continue;
            };
            let template: &ast::ValueSpec = if vs.values.is_empty() {
                previous.ok_or("missing constant value")?
            } else {
                previous = Some(vs);
                vs
            };
            if template.values.len() != vs.names.len() {
                return Err("const declaration needs one value per name".into());
            }
            self.push_scope();
            self.bind(
                "iota",
                Binding::Const(Val::int(Ty::UntypedInt, index as i128)),
            );
            let result = self.const_values(vs, template);
            self.pop_scope();
            for (name, val) in result? {
                self.bind(&name, Binding::Const(val));
            }
        }
        Ok(())
    }

    fn const_values(
        &mut self,
        vs: &ast::ValueSpec,
        template: &ast::ValueSpec,
    ) -> R<Vec<(String, Val)>> {
        let objs = self.objs;
        let want = match &template.typ {
            Some(t) => Some(self.resolve_type(t)?),
            None => None,
        };
        let mut out = Vec::new();
        for (name, value) in vs.names.iter().zip(&template.values) {
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

    /// Declares package-level variables. A constant initializer becomes the
    /// global's initial value; anything else is queued for the init function.
    fn global_var_spec(&mut self, vs: &'a ast::ValueSpec) -> R<()> {
        let objs = self.objs;
        let want = match &vs.typ {
            Some(t) => Some(self.resolve_type(t)?),
            None => None,
        };
        if vs.values.is_empty() {
            let ty = want.ok_or("package-level var needs a type or a value")?;
            for name in &vs.names {
                self.define_global(&objs.idents[*name].name, &ty, "zeroinitializer");
            }
            return Ok(());
        }
        if vs.values.len() != vs.names.len() {
            if vs.values.len() != 1 {
                return Err("package-level var needs one value per name".into());
            }
            self.pending_globals.push((vs, usize::MAX));
            return Ok(());
        }
        for (i, name) in vs.names.iter().enumerate() {
            // Try the initializer as a constant in a scratch context. Failure
            // is not an error yet: it may call a function declared later.
            self.f = FnCtx::new(Ty::Void, HashSet::new());
            let attempt = match &want {
                Some(ty) => self.expr_want(&vs.values[i], ty),
                None => self.expr_m(&vs.values[i]),
            };
            let constant = self.f.body.is_empty() && self.f.nested_defs.is_empty();
            self.f = FnCtx::new(Ty::Void, HashSet::new());
            self.err_located = false;
            match attempt {
                Ok(val) if constant => {
                    self.define_global(&objs.idents[*name].name, &val.ty, &val.repr);
                }
                _ => self.pending_globals.push((vs, i)),
            }
        }
        Ok(())
    }

    /// Resolves the parameter and result types of a function type.
    fn signature(&mut self, ft: &ast::FuncType) -> R<Signature> {
        let objs = self.objs;
        let mut params = Vec::new();
        let mut variadic = false;
        for field_key in &ft.params.list {
            let field = &objs.fields[*field_key];
            if variadic {
                return Err("only the last parameter can be variadic".into());
            }
            let ty = match &field.typ {
                Expr::Ellipsis(e) => {
                    variadic = true;
                    let elem = e.elt.as_ref().ok_or("variadic parameter needs a type")?;
                    Ty::Slice(Box::new(self.resolve_type(elem)?))
                }
                other => self.resolve_type(other)?,
            };
            if field.names.is_empty() {
                params.push(("_".to_string(), ty));
            } else {
                for n in &field.names {
                    params.push((objs.idents[*n].name.clone(), ty.clone()));
                }
            }
        }

        let mut result_types = Vec::new();
        let mut results = Vec::new();
        if let Some(list) = &ft.results {
            for field_key in &list.list {
                let field = &objs.fields[*field_key];
                let ty = self.resolve_type(&field.typ)?;
                if field.names.is_empty() {
                    result_types.push(ty);
                } else {
                    for n in &field.names {
                        results.push(objs.idents[*n].name.clone());
                        result_types.push(ty.clone());
                    }
                }
            }
        }
        if !results.is_empty() && results.len() != result_types.len() {
            return Err("cannot mix named and unnamed results".into());
        }
        let ret = match result_types.len() {
            0 => Ty::Void,
            1 => result_types.pop().unwrap(),
            _ => Ty::Tuple(result_types),
        };
        Ok(Signature {
            params,
            results,
            ret,
            variadic,
        })
    }

    fn func_shape(&mut self, fd: &ast::FuncDecl) -> R<Shape> {
        let objs = self.objs;
        let name = objs.idents[fd.name].name.clone();
        let mut sig = self.signature(&objs.ftypes[fd.typ])?;
        let mut key = self.q(&name);
        let mut ptr_recv = None;

        if let Some(recv) = &fd.recv {
            let Some(field_key) = recv.list.first() else {
                return Err(format!("{name}: method has no receiver"));
            };
            let field = &objs.fields[*field_key];
            let ty = self.resolve_type(&field.typ)?;
            let (base, is_ptr) = match &ty {
                Ty::Ptr(inner) => (inner.as_ref(), true),
                other => (other, false),
            };
            let Some(type_name) = base.type_name() else {
                return Err(format!("{name}: invalid receiver type {ty}"));
            };
            key = format!("{type_name}.{name}");
            ptr_recv = Some(is_ptr);
            let recv_name = match field.names.first() {
                Some(n) => objs.idents[*n].name.clone(),
                None => "_".to_string(),
            };
            sig.params.insert(0, (recv_name, ty));
        }

        Ok(Shape { key, sig, ptr_recv })
    }

    fn compile_func(&mut self, fd: &ast::FuncDecl, key: &str) -> R<()> {
        let objs = self.objs;
        let name = objs.idents[fd.name].name.as_str();
        let pos = objs.idents[fd.name].pos;
        let Some(body) = &fd.body else {
            // Declared without a body: implemented in C.
            return Ok(());
        };
        let shape = self.func_shape(fd);
        let shape = shape.map_err(|e| self.locate(pos, e))?;
        let symbol = self.funcs[key].symbol.clone();
        let exported = self.pkg.is_empty()
            && shape.ptr_recv.is_none()
            && export_name(self.source, name).is_some();
        let def = self.lower_function(&symbol, &shape.sig, &body.list, exported, false);
        let def = def.map_err(|e| self.locate(pos, e))?;
        let nested = std::mem::take(&mut self.f.nested_defs);
        self.fn_defs.extend(nested);
        self.fn_defs.push(def);
        Ok(())
    }

    /// Lowers one function body and returns its definition. `self.f` is left
    /// holding the final state, so the caller can read captures and closures.
    fn lower_function(
        &mut self,
        symbol: &str,
        sig: &Signature,
        body: &[Stmt],
        exported: bool,
        closure: bool,
    ) -> R<String> {
        let result_types = match &sig.ret {
            Ty::Tuple(items) => items.clone(),
            Ty::Void => Vec::new(),
            other => vec![other.clone()],
        };

        // Lower again while the set of address-taken locals grows, or when a
        // defer statement shows that the function needs a defer frame.
        let mut escaping = HashSet::new();
        let mut uses_defer = false;
        loop {
            if uses_defer {
                // Deferred calls and recovery read named results from memory.
                escaping.extend(sig.results.iter().cloned());
            }
            self.f = FnCtx::new(sig.ret.clone(), escaping);
            self.f.symbol = symbol.to_string();
            self.f.uses_defer = uses_defer;
            self.f.scopes.push(HashMap::new());
            for (i, (pname, ty)) in sig.params.iter().enumerate() {
                if pname != "_" {
                    let arg = Val::new(ty.clone(), format!("%arg{i}"));
                    self.declare(pname, ty.clone(), Some(arg));
                }
            }
            for (rname, ty) in sig.results.iter().zip(&result_types) {
                self.declare(rname, ty.clone(), None);
            }
            self.f.named_results = sig.results.clone();
            if uses_defer {
                self.defer_prologue(&result_types);
            }
            self.block(body)?;
            if uses_defer {
                if !self.f.terminated && sig.ret != Ty::Void {
                    self.emit("unreachable".into());
                    self.f.terminated = true;
                }
                self.defer_epilogue(&sig.ret);
            } else if !self.f.terminated {
                if sig.ret == Ty::Void {
                    self.emit("ret void".into());
                } else {
                    self.emit("unreachable".into());
                }
            }
            if !self.f.retry {
                break;
            }
            uses_defer |= self.f.wants_defer;
            escaping = std::mem::take(&mut self.f.escaping);
        }

        let mut params: Vec<String> = sig
            .params
            .iter()
            .enumerate()
            .map(|(i, (_, ty))| format!("{} %arg{i}", ty.ll()))
            .collect();
        if closure {
            params.insert(0, "ptr %ctx".to_string());
        }
        let linkage = if exported { "" } else { "internal " };
        let mut def = format!(
            "define {linkage}{} @\"{symbol}\"({}) {{\nentry:\n",
            sig.ret.ll(),
            params.join(", ")
        );
        // A closure reads the addresses of its captured variables from its context.
        for i in 0..self.f.captures.len() {
            let _ = writeln!(
                def,
                "  %cap{i}.slot = getelementptr inbounds ptr, ptr %ctx, i64 {i}\n  %cap{i} = load ptr, ptr %cap{i}.slot"
            );
        }
        for line in self.f.allocas.iter().chain(&self.f.body) {
            def.push_str(line);
            def.push('\n');
        }
        def.push_str("}\n");
        Ok(def)
    }

    /// Sets up the defer frame. `_setjmp` returns a second time, with a
    /// non-zero value, when a deferred call recovers from a panic; the
    /// function then leaves through `fn.exit` like a normal return.
    fn defer_prologue(&mut self, result_types: &[Ty]) {
        self.f.allocas.push("  %frame = alloca [3 x ptr]".into());
        self.f
            .allocas
            .push("  %env = alloca [512 x i8], align 16".into());
        let names = self.f.named_results.clone();
        for (i, ty) in result_types.iter().enumerate() {
            let slot = match names.get(i).and_then(|name| self.lookup(name)) {
                Some(Binding::Var { ptr, .. }) => ptr,
                _ => {
                    let slot = self.alloca(ty);
                    self.store(&Val::new(ty.clone(), "zeroinitializer"), &slot);
                    slot
                }
            };
            self.f.ret_slots.push((slot, ty.clone()));
        }
        self.emit("call void @goiaba_frame_enter(ptr %frame, ptr %env)".into());
        let again = self.ins("call i32 @_setjmp(ptr %env) returns_twice".into());
        let recovered = self.ins(format!("icmp ne i32 {again}, 0"));
        self.cond_br(&recovered, "fn.exit", "fn.body");
        self.place_label("fn.body");
    }

    /// Runs the deferred calls and returns the stored results.
    fn defer_epilogue(&mut self, ret: &Ty) {
        if !self.f.terminated {
            self.br("fn.exit");
        }
        self.place_label("fn.exit");
        self.emit("call void @goiaba_frame_exit(ptr %frame)".into());
        let slots = self.f.ret_slots.clone();
        // Volatile, so the values survive the jump back from a recovered panic.
        let vals: Vec<Val> = slots
            .iter()
            .map(|(slot, ty)| {
                let name = self.ins(format!("load volatile {}, ptr {slot}", ty.ll()));
                Val::new(ty.clone(), name)
            })
            .collect();
        self.emit_ret(ret, &vals);
    }

    fn emit_ret(&mut self, ret: &Ty, vals: &[Val]) {
        match vals {
            [] => self.emit("ret void".into()),
            [val] => self.emit(format!("ret {} {}", ret.ll(), val.repr)),
            _ => {
                let ll = ret.ll();
                let mut cur = "poison".to_string();
                for (i, val) in vals.iter().enumerate() {
                    cur = self.ins(format!(
                        "insertvalue {ll} {cur}, {} {}, {i}",
                        val.ty.ll(),
                        val.repr
                    ));
                }
                self.emit(format!("ret {ll} {cur}"));
            }
        }
        self.f.terminated = true;
    }

    /// `defer f(args)`: the callee and arguments are evaluated now and saved
    /// in a record; a small thunk performs the call when the function exits.
    fn defer_stmt(&mut self, d: &ast::DeferStmt) -> R<()> {
        let Expr::Call(call) = strip_parens(&d.call) else {
            return Err("defer needs a function call".into());
        };
        if !self.f.uses_defer {
            self.f.wants_defer = true;
            self.f.retry = true;
            return Ok(());
        }
        let record = self.call_record(call, "defer")?;
        self.emit(format!(
            "call void @goiaba_defer_push(ptr %frame, ptr {record})"
        ));
        Ok(())
    }

    /// Evaluates a call without performing it and returns a heap record
    /// `{ next, thunk, operands... }` whose thunk makes the call.
    fn call_record(&mut self, call: &ast::CallExpr, what: &str) -> R<String> {
        self.f.pending = None;
        self.call_mode(call, true)?;
        let Some(pending) = self.f.pending.take() else {
            return Err(format!(
                "{what} of a builtin is not supported yet; wrap it in a function literal"
            ));
        };
        let fields: String = pending
            .args
            .iter()
            .map(|(ty, _)| format!(", {ty}"))
            .collect();
        let record_ty = format!("{{ ptr, ptr{fields} }}");
        self.f.closure_count += 1;
        let thunk = format!("{}$call{}", self.f.symbol, self.f.closure_count);

        let mut def = format!("define internal void @\"{thunk}\"(ptr %rec) {{\nentry:\n");
        let mut operands = Vec::new();
        for (i, (ty, _)) in pending.args.iter().enumerate() {
            let _ = writeln!(
                def,
                "  %p{i} = getelementptr inbounds {record_ty}, ptr %rec, i32 0, i32 {}\n  %v{i} = load {ty}, ptr %p{i}",
                i + 2
            );
            operands.push(format!("{ty} %v{i}"));
        }
        let callee = match &pending.symbol {
            Some(symbol) => symbol.clone(),
            None => {
                operands.remove(0);
                "%v0".to_string()
            }
        };
        let ret = pending.ret.ll();
        let result = if pending.ret == Ty::Void { "" } else { "%r = " };
        let _ = write!(
            def,
            "  {result}call {ret} {callee}({})\n  ret void\n}}\n",
            operands.join(", ")
        );
        self.f.nested_defs.push(def);

        let record = self.ins(format!(
            "call ptr @goiaba_alloc(i64 ptrtoint (ptr getelementptr ({record_ty}, ptr null, i32 1) to i64))"
        ));
        let slot = self.ins(format!(
            "getelementptr inbounds {record_ty}, ptr {record}, i32 0, i32 1"
        ));
        self.emit(format!("store ptr @\"{thunk}\", ptr {slot}"));
        for (i, (ty, value)) in pending.args.iter().enumerate() {
            let slot = self.ins(format!(
                "getelementptr inbounds {record_ty}, ptr {record}, i32 0, i32 {}",
                i + 2
            ));
            self.emit(format!("store {ty} {value}, ptr {slot}"));
        }
        Ok(record)
    }

    /// Emits a call, or saves it in `self.f.pending` when `capture` is set.
    /// `symbol` is `None` when the code pointer is the first of `args`.
    fn finish_call(
        &mut self,
        ret: &Ty,
        symbol: Option<String>,
        args: Vec<(String, String)>,
        capture: bool,
    ) -> Val {
        if capture {
            self.f.pending = Some(Pending {
                ret: ret.clone(),
                symbol,
                args,
            });
            return Val::void();
        }
        let mut operands: Vec<String> = args.iter().map(|(ty, v)| format!("{ty} {v}")).collect();
        let callee = match symbol {
            Some(symbol) => symbol,
            None => {
                operands.remove(0);
                args[0].1.clone()
            }
        };
        let target = format!("{} {callee}({})", ret.ll(), operands.join(", "));
        if *ret == Ty::Void {
            self.emit(format!("call {target}"));
            Val::void()
        } else {
            let result = self.ins(format!("call {target}"));
            Val::new(ret.clone(), result)
        }
    }

    /// Lowers a function literal to a closure value.
    fn func_lit(&mut self, lit: &ast::FuncLit) -> R<Val> {
        let sig = self.signature(&self.objs.ftypes[lit.typ])?;
        self.f.closure_count += 1;
        let symbol = format!("{}$f{}", self.f.symbol, self.f.closure_count);
        let ty = Ty::Func(Box::new(sig.func_ty()));

        let parent = std::mem::replace(&mut self.f, FnCtx::new(Ty::Void, HashSet::new()));
        self.outer.push(parent);
        let result = self.lower_function(&symbol, &sig, &lit.body.list, false, true);
        let parent = self.outer.pop().expect("closure has an enclosing function");
        let child = std::mem::replace(&mut self.f, parent);
        let def = result?;
        self.f.nested_defs.extend(child.nested_defs);
        self.f.nested_defs.push(def);

        if child.captures.is_empty() {
            return Ok(Val::new(ty, format!("{{ ptr @\"{symbol}\", ptr null }}")));
        }
        let count = child.captures.len().to_string();
        let ctx = self.alloc_array(&count, &Ty::Nil);
        for (i, cap) in child.captures.iter().enumerate() {
            let ptr = if cap.indirect {
                self.ins(format!("load ptr, ptr {}", cap.ptr))
            } else {
                cap.ptr.clone()
            };
            let slot = self.ins(format!("getelementptr inbounds ptr, ptr {ctx}, i64 {i}"));
            self.emit(format!("store ptr {ptr}, ptr {slot}"));
        }
        let a = self.ins(format!("insertvalue %func poison, ptr @\"{symbol}\", 0"));
        let b = self.ins(format!("insertvalue %func {a}, ptr {ctx}, 1"));
        Ok(Val::new(ty, b))
    }

    /// A top-level function used as a value. Function values are called with
    /// a leading context pointer, so the function gets a thunk that drops it.
    fn func_value(&mut self, name: &str) -> R<Val> {
        let sig = self.funcs[name].clone();
        let thunk = format!("{}$thunk", sig.symbol);
        if self.thunks.insert(thunk.clone()) {
            let mut params = vec!["ptr %ctx".to_string()];
            let mut args = Vec::new();
            for (i, ty) in sig.params.iter().enumerate() {
                params.push(format!("{} %arg{i}", ty.ll()));
                args.push(format!("{} %arg{i}", ty.ll()));
            }
            let ret = sig.ret.ll();
            let call = format!("call {ret} @\"{}\"({})", sig.symbol, args.join(", "));
            let body = if sig.ret == Ty::Void {
                format!("  {call}\n  ret void")
            } else {
                format!("  %r = {call}\n  ret {ret} %r")
            };
            self.fn_defs.push(format!(
                "define internal {ret} @\"{thunk}\"({}) {{\nentry:\n{body}\n}}\n",
                params.join(", ")
            ));
        }
        let ty = Ty::Func(Box::new(FuncTy {
            params: sig.params.clone(),
            ret: sig.ret.clone(),
            variadic: sig.variadic,
        }));
        Ok(Val::new(ty, format!("{{ ptr @\"{thunk}\", ptr null }}")))
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
        self.ins(format!("call ptr @goiaba_alloc(i64 {})", sizeof(ty)))
    }

    /// Allocates zeroed room for `count` values of `elem`.
    fn alloc_array(&mut self, count: &str, elem: &Ty) -> String {
        let bytes = self.ins(format!("mul i64 {count}, {}", sizeof(elem)));
        self.ins(format!("call ptr @goiaba_alloc(i64 {bytes})"))
    }

    fn nilcheck(&mut self, ptr: &str) {
        self.emit(format!("call void @goiaba.nilcheck(ptr {ptr})"));
    }

    fn make_string(&mut self, data: &str, len: &str) -> Val {
        let a = self.ins(format!("insertvalue %string poison, ptr {data}, 0"));
        let b = self.ins(format!("insertvalue %string {a}, i64 {len}, 1"));
        Val::new(Ty::Str, b)
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

    fn lookup(&mut self, name: &str) -> Option<Binding> {
        let local = self.f.scopes.iter().rev().find_map(|scope| scope.get(name));
        if let Some(binding) = local {
            return Some(binding.clone());
        }
        if let Some(binding) = self.capture(name, self.outer.len()) {
            return Some(binding);
        }
        self.globals.get(&self.q(name)).cloned()
    }

    /// Resolves `name` for the function at nesting `level` by looking in its
    /// enclosing functions, and records it as a capture. Level 0 is the
    /// outermost function; `self.outer.len()` is the one being lowered.
    fn capture(&mut self, name: &str, level: usize) -> Option<Binding> {
        if level == 0 {
            return None;
        }
        let parent = &self.outer[level - 1];
        let found = parent
            .scopes
            .iter()
            .rev()
            .find_map(|s| s.get(name))
            .cloned();
        let found = match found {
            Some(binding) => binding,
            None => self.capture(name, level - 1)?,
        };
        let Binding::Var {
            ptr,
            ty,
            heap,
            indirect,
        } = found
        else {
            return Some(found);
        };
        if !heap {
            // A captured variable must outlive its frame.
            let parent = &mut self.outer[level - 1];
            if parent.escaping.insert(name.to_string()) {
                parent.retry = true;
            }
        }
        let ctx = if level == self.outer.len() {
            &mut self.f
        } else {
            &mut self.outer[level]
        };
        let local = format!("%cap{}", ctx.captures.len());
        ctx.captures.push(Capture { ptr, indirect });
        let binding = Binding::Var {
            ptr: local,
            ty,
            heap: true,
            indirect: false,
        };
        ctx.scopes[0].insert(name.to_string(), binding.clone());
        Some(binding)
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
                let key = self.q(name);
                self.globals.insert(key, binding);
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
        self.bind(
            name,
            Binding::Var {
                ptr,
                ty,
                heap,
                indirect: false,
            },
        );
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

    /// Implicit conversion for assignment. Untyped constants and nil take the
    /// target type; a defined type and an unnamed composite type with the same
    /// underlying type are also assignable to each other.
    fn coerce(&mut self, val: Val, to: &Ty) -> R<Val> {
        if val.ty == *to {
            return Ok(val);
        }
        if matches!(to.under(), Ty::Iface(_)) {
            return if val.ty == Ty::Nil {
                Ok(Val::new(to.clone(), "zeroinitializer"))
            } else {
                self.box_iface(val, to)
            };
        }
        if val.ty.is_untyped_num() {
            return self.convert(val, to);
        }
        match (&val.ty, to.under()) {
            (Ty::Nil, Ty::Ptr(_) | Ty::Map(..)) => return Ok(Val::new(to.clone(), "null")),
            (Ty::Nil, Ty::Slice(_) | Ty::Func(_)) => {
                return Ok(Val::new(to.clone(), "zeroinitializer"));
            }
            _ => {}
        }
        let named = |ty: &Ty| matches!(ty, Ty::Named(..) | Ty::Struct(_));
        let composite = matches!(
            to.under(),
            Ty::Ptr(_) | Ty::Slice(_) | Ty::Array(..) | Ty::Tuple(_) | Ty::Func(_) | Ty::Map(..)
        );
        if composite && val.ty.under() == to.under() && !(named(&val.ty) && named(to)) {
            return Ok(Val {
                ty: to.clone(),
                ..val
            });
        }
        Err(format!("cannot use {} as {to}", val.ty))
    }

    /// Explicit conversion `T(x)`.
    fn convert(&mut self, val: Val, to: &Ty) -> R<Val> {
        if val.ty == *to {
            return Ok(val);
        }
        if matches!(to.under(), Ty::Iface(_)) || val.ty == Ty::Nil {
            return self.coerce(val, to);
        }
        let byte = Ty::Int {
            bits: 8,
            signed: false,
        };
        if val.ty.is_untyped_num() {
            let k = val.k.ok_or("internal: untyped value without a constant")?;
            return match (to.under(), k) {
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
                (Ty::Float(bits), k) => Ok(Val {
                    ty: to.clone(),
                    ..Val::float(Ty::Float(*bits), k.as_f64())
                }),
                (Ty::Str, K::Int(_)) => {
                    let rune = self.materialize(Val {
                        ty: Ty::UntypedRune,
                        ..val
                    })?;
                    self.convert(rune, to)
                }
                _ => Err(format!("cannot convert {} constant to {to}", val.ty)),
            };
        }
        // Identical underlying types share a representation.
        if val.ty.under() == to.under() {
            return Ok(Val {
                ty: to.clone(),
                ..val
            });
        }
        let (from_ll, to_ll) = (val.ty.ll(), to.ll());
        let cast = match (val.ty.under(), to.under()) {
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
            (Ty::Int { .. }, Ty::Str) => {
                // string(rune): the UTF-8 encoding of one code point.
                let rune = self.convert(
                    val,
                    &Ty::Int {
                        bits: 32,
                        signed: true,
                    },
                )?;
                let buf = self.alloca(&Ty::Array(Box::new(byte), 4));
                let len = self.ins(format!(
                    "call i64 @goiaba_encode_rune(i32 {}, ptr {buf})",
                    rune.repr
                ));
                let data = self.ins(format!("call ptr @goiaba.clone(ptr {buf}, i64 {len})"));
                let s = self.make_string(&data, &len);
                return Ok(Val::new(to.clone(), s.repr));
            }
            (Ty::Slice(elem), Ty::Str) if **elem == byte => {
                let data = self.ins(format!("extractvalue %slice {}, 0", val.repr));
                let len = self.ins(format!("extractvalue %slice {}, 1", val.repr));
                let copy = self.ins(format!("call ptr @goiaba.clone(ptr {data}, i64 {len})"));
                let s = self.make_string(&copy, &len);
                return Ok(Val::new(to.clone(), s.repr));
            }
            (Ty::Str, Ty::Slice(elem)) if **elem == byte => {
                let data = self.ins(format!("extractvalue %string {}, 0", val.repr));
                let len = self.ins(format!("extractvalue %string {}, 1", val.repr));
                let copy = self.ins(format!("call ptr @goiaba.clone(ptr {data}, i64 {len})"));
                return Ok(self.make_slice(to.clone(), &copy, &len, &len));
            }
            _ => return Err(format!("cannot convert {} to {to}", val.ty)),
        };
        let name = self.ins(format!("{cast} {from_ll} {} to {to_ll}", val.repr));
        Ok(Val::new(to.clone(), name))
    }

    // ---- interfaces -------------------------------------------------------

    /// The methods callable on a value of type `ty`, sorted by name. A value
    /// of type T has the value-receiver methods; *T has all of them.
    fn method_set(&self, ty: &Ty) -> Vec<(String, FuncSig)> {
        let (base, is_ptr) = match ty {
            Ty::Ptr(inner) => (inner.as_ref(), true),
            other => (other, false),
        };
        let Some(type_name) = base.type_name() else {
            return Vec::new();
        };
        let mut set = Vec::new();
        for method in self.methods.get(type_name).into_iter().flatten() {
            let sig = &self.funcs[&format!("{type_name}.{method}")];
            if is_ptr || sig.ptr_recv == Some(false) {
                set.push((method.clone(), sig.clone()));
            }
        }
        set.sort_by(|a, b| a.0.cmp(&b.0));
        set
    }

    /// The symbol to call for a method through an interface: `fn(ptr data, args...)`.
    /// A pointer-receiver method already has that shape. A value-receiver
    /// method gets a wrapper that loads the receiver from the data pointer.
    fn iface_fn(&mut self, sig: &FuncSig) -> String {
        if sig.ptr_recv == Some(true) {
            return sig.symbol.clone();
        }
        let wrapper = format!("{}$iface", sig.symbol);
        if self.thunks.insert(wrapper.clone()) {
            let recv = sig.params[0].ll();
            let mut params = vec!["ptr %data".to_string()];
            let mut args = vec![format!("{recv} %recv")];
            for (i, ty) in sig.params.iter().enumerate().skip(1) {
                params.push(format!("{} %arg{i}", ty.ll()));
                args.push(format!("{} %arg{i}", ty.ll()));
            }
            let ret = sig.ret.ll();
            let call = format!("call {ret} @\"{}\"({})", sig.symbol, args.join(", "));
            let tail = if sig.ret == Ty::Void {
                format!("  {call}\n  ret void")
            } else {
                format!("  %r = {call}\n  ret {ret} %r")
            };
            self.fn_defs.push(format!(
                "define internal {ret} @\"{wrapper}\"({}) {{\nentry:\n  call void @goiaba.nilcheck(ptr %data)\n  %recv = load {recv}, ptr %data\n{tail}\n}}\n",
                params.join(", ")
            ));
        }
        wrapper
    }

    /// Checks that `ty` implements the interface and returns the functions
    /// that fill its method table, in the interface's method order.
    fn implements(&mut self, ty: &Ty, methods: &[(String, FuncTy)], iface: &Ty) -> R<Vec<String>> {
        let set = self.method_set(ty);
        let mut fns = Vec::new();
        for (name, want) in methods {
            let Some((_, sig)) = set.iter().find(|(n, _)| n == name) else {
                let hint = match ty.type_name() {
                    Some(t) if self.funcs.contains_key(&format!("{t}.{name}")) => {
                        format!("method {name} has pointer receiver")
                    }
                    _ => format!("missing method {name}"),
                };
                return Err(format!("{ty} does not implement {iface} ({hint})"));
            };
            if sig.params[1..] != want.params[..]
                || sig.ret != want.ret
                || sig.variadic != want.variadic
            {
                return Err(format!(
                    "{ty} does not implement {iface} (wrong type for method {name})"
                ));
            }
            fns.push(self.iface_fn(sig));
        }
        Ok(fns)
    }

    /// The global describing `ty` at run time: kind, size, name, equality
    /// function, method table, and enough structure (element type, fields)
    /// for the runtime to walk a value. Its address identifies the type.
    fn rtype_of(&mut self, ty: &Ty) -> String {
        let name = ty.go_name();
        if let Some(symbol) = self.rtypes.get(&name) {
            return symbol.clone();
        }
        let symbol = format!("@\"type.{name}\"");
        // Registered first, so a recursive type can refer to itself.
        self.rtypes.insert(name.clone(), symbol.clone());

        // Kinds and flags are mirrored in runtime.c.
        let (kind, mut flags) = match ty.under() {
            Ty::Bool => (1, 0),
            Ty::Int { signed: true, .. } => (2, 0),
            Ty::Int { signed: false, .. } => (3, 0),
            Ty::Float(_) => (4, 0),
            Ty::Str => (5, 0),
            Ty::Ptr(_) => (6, 1),
            Ty::Slice(_) => (9, 0),
            Ty::Array(..) => (10, 0),
            Ty::Struct(_) => (11, 0),
            Ty::Map(..) => (12, 0),
            Ty::Iface(_) => (13, 0),
            Ty::Func(_) => (14, 0),
            _ => (7, 0),
        };
        if matches!(ty.under(), Ty::Iface(methods) if !methods.is_empty()) {
            flags |= 2;
        }
        // elem: element or pointee type. count: array length or field count.
        // extra: the field table of a struct, or the key type of a map.
        let (elem, count, extra) = match ty.under().clone() {
            Ty::Ptr(inner) | Ty::Slice(inner) => (self.rtype_of(&inner), 0, "null".to_string()),
            Ty::Array(inner, n) => (self.rtype_of(&inner), n, "null".to_string()),
            Ty::Map(key, val) => {
                let key = self.rtype_of(&key);
                (self.rtype_of(&val), 0, key)
            }
            Ty::Struct(struct_name) => {
                let fields = self.structs[&struct_name].clone();
                let ll = ty.ll();
                let mut entries = Vec::new();
                for (i, (field, field_ty)) in fields.iter().enumerate() {
                    let field_type = self.rtype_of(field_ty);
                    let text = self.intern(field.as_bytes().to_vec());
                    entries.push(format!(
                        "%rfield {{ ptr {text}, i64 {}, ptr {field_type}, i64 ptrtoint (ptr getelementptr ({ll}, ptr null, i32 0, i32 {i}) to i64) }}",
                        field.len()
                    ));
                }
                if entries.is_empty() {
                    ("null".to_string(), 0, "null".to_string())
                } else {
                    let table = format!("@\"type.{name}.fields\"");
                    self.global_defs.push(format!(
                        "{table} = private constant [{} x %rfield] [{}]",
                        entries.len(),
                        entries.join(", ")
                    ));
                    ("null".to_string(), entries.len() as u64, table)
                }
            }
            _ => ("null".to_string(), 0, "null".to_string()),
        };

        let equal = self.eq_fn(ty);
        let set = self.method_set(ty);
        let mut entries = Vec::new();
        for (method, sig) in &set {
            let code = self.iface_fn(sig);
            let text = self.intern(method.as_bytes().to_vec());
            entries.push(format!(
                "%rmethod {{ ptr {text}, i64 {}, ptr @\"{code}\" }}",
                method.len()
            ));
        }
        let table = if entries.is_empty() {
            "null".to_string()
        } else {
            let table = format!("@\"type.{name}.methods\"");
            self.global_defs.push(format!(
                "{table} = private constant [{} x %rmethod] [{}]",
                entries.len(),
                entries.join(", ")
            ));
            table
        };
        let text = self.intern(name.as_bytes().to_vec());
        self.global_defs.push(format!(
            "{symbol} = private constant %rtype {{ i32 {kind}, i32 {flags}, i64 {}, ptr {text}, i64 {}, ptr {equal}, i64 {}, ptr {table}, ptr {elem}, i64 {count}, ptr {extra} }}",
            sizeof(ty),
            name.len(),
            entries.len()
        ));
        symbol
    }

    /// A function `i32 (ptr, ptr)` comparing two values of `ty` in memory,
    /// or `null` when the type is not comparable.
    fn eq_fn(&mut self, ty: &Ty) -> String {
        let name = ty.go_name();
        if let Some(symbol) = self.eq_fns.get(&name) {
            return symbol.clone();
        }
        let symbol = format!("@\"eq.{name}\"");
        // Registered first so a recursive type refers to its own function.
        self.eq_fns.insert(name.clone(), symbol.clone());

        let saved = std::mem::replace(&mut self.f, FnCtx::new(Ty::Bool, HashSet::new()));
        let a = self.load("%a", ty);
        let b = self.load("%b", ty);
        let result = self.value_eq(ty, &a.repr, &b.repr);
        let ctx = std::mem::replace(&mut self.f, saved);
        let Ok(equal) = result else {
            self.eq_fns.insert(name, "null".to_string());
            return "null".to_string();
        };
        let mut def = format!("define internal i32 {symbol}(ptr %a, ptr %b) {{\nentry:\n");
        for line in ctx.allocas.iter().chain(&ctx.body) {
            def.push_str(line);
            def.push('\n');
        }
        let _ = write!(def, "  %r = zext i1 {equal} to i32\n  ret i32 %r\n}}\n");
        self.fn_defs.push(def);
        symbol
    }

    /// Emits a comparison of two values of `ty` and returns the i1 result.
    fn value_eq(&mut self, ty: &Ty, a: &str, b: &str) -> R<String> {
        let ll = ty.ll();
        match ty.under() {
            Ty::Int { .. } | Ty::Bool | Ty::Ptr(_) => {
                Ok(self.ins(format!("icmp eq {ll} {a}, {b}")))
            }
            Ty::Float(_) => Ok(self.ins(format!("fcmp oeq {ll} {a}, {b}"))),
            Ty::Str => {
                let cmp = self.ins(format!("call i32 @goiaba.strcmp(%string {a}, %string {b})"));
                Ok(self.ins(format!("icmp eq i32 {cmp}, 0")))
            }
            Ty::Struct(name) => {
                let fields = self.structs[name].clone();
                let mut all = "true".to_string();
                for (i, (_, field_ty)) in fields.iter().enumerate() {
                    let fa = self.ins(format!("extractvalue {ll} {a}, {i}"));
                    let fb = self.ins(format!("extractvalue {ll} {b}, {i}"));
                    let equal = self.value_eq(field_ty, &fa, &fb)?;
                    all = self.ins(format!("and i1 {all}, {equal}"));
                }
                Ok(all)
            }
            Ty::Array(elem, n) if *n <= 16 => {
                let elem = elem.as_ref().clone();
                let mut all = "true".to_string();
                for i in 0..*n {
                    let ea = self.ins(format!("extractvalue {ll} {a}, {i}"));
                    let eb = self.ins(format!("extractvalue {ll} {b}, {i}"));
                    let equal = self.value_eq(&elem, &ea, &eb)?;
                    all = self.ins(format!("and i1 {all}, {equal}"));
                }
                Ok(all)
            }
            Ty::Iface(methods) => {
                let named = !methods.is_empty();
                let (ta, da) = self.iface_parts(a, named);
                let (tb, db) = self.iface_parts(b, named);
                let equal = self.ins(format!(
                    "call i32 @goiaba_iface_eq(ptr {ta}, ptr {da}, ptr {tb}, ptr {db})"
                ));
                Ok(self.ins(format!("icmp ne i32 {equal}, 0")))
            }
            _ => Err(format!("{ty} is not comparable")),
        }
    }

    /// Splits an interface value into its dynamic type descriptor and data
    /// pointer. `has_methods` says whether the first word is a method table.
    fn iface_parts(&mut self, repr: &str, has_methods: bool) -> (String, String) {
        let word = self.ins(format!("extractvalue %iface {repr}, 0"));
        let data = self.ins(format!("extractvalue %iface {repr}, 1"));
        let rtype = if has_methods {
            self.ins(format!("call ptr @goiaba.itab_type(ptr {word})"))
        } else {
            word
        };
        (rtype, data)
    }

    fn make_iface(&mut self, ty: &Ty, word: &str, data: &str) -> Val {
        let a = self.ins(format!("insertvalue %iface poison, ptr {word}, 0"));
        let b = self.ins(format!("insertvalue %iface {a}, ptr {data}, 1"));
        Val::new(ty.clone(), b)
    }

    /// The global listing an interface's method names, used to build method
    /// tables at run time.
    fn iface_desc(&mut self, iface: &Ty, methods: &[(String, FuncTy)]) -> String {
        let name = iface.go_name();
        let symbol = format!("@\"iface.{name}\"");
        if self.iface_globals.insert(symbol.clone()) {
            let mut entries = Vec::new();
            for (method, _) in methods {
                let text = self.intern(method.as_bytes().to_vec());
                entries.push(format!(
                    "{{ ptr, i64 }} {{ ptr {text}, i64 {} }}",
                    method.len()
                ));
            }
            let list = format!("@\"iface.{name}.methods\"");
            self.global_defs.push(format!(
                "{list} = private constant [{} x {{ ptr, i64 }}] [{}]",
                entries.len(),
                entries.join(", ")
            ));
            let text = self.intern(name.as_bytes().to_vec());
            self.global_defs.push(format!(
                "{symbol} = private constant {{ i64, ptr, ptr, i64 }} {{ i64 {}, ptr {list}, ptr {text}, i64 {} }}",
                methods.len(),
                name.len()
            ));
        }
        symbol
    }

    /// Converts a value to interface type `to`.
    fn box_iface(&mut self, val: Val, to: &Ty) -> R<Val> {
        let Ty::Iface(methods) = to.under().clone() else {
            return Err(format!("internal: {to} is not an interface"));
        };
        if let Ty::Iface(source) = val.ty.under().clone() {
            if source == methods {
                return Ok(Val {
                    ty: to.clone(),
                    ..val
                });
            }
            let covered = methods.iter().all(|m| source.contains(m));
            if !covered {
                return Err(format!(
                    "cannot use {} as {to}: need a type assertion",
                    val.ty
                ));
            }
            let (rtype, data) = self.iface_parts(&val.repr, !source.is_empty());
            if methods.is_empty() {
                return Ok(self.make_iface(to, &rtype, &data));
            }
            let desc = self.iface_desc(to, &methods);
            let tab = self.ins(format!("call ptr @goiaba_itab(ptr {desc}, ptr {rtype})"));
            return Ok(self.make_iface(to, &tab, &data));
        }

        let val = self.materialize(val)?;
        let fns = self.implements(&val.ty, &methods, to)?;
        let rtype = self.rtype_of(&val.ty);
        let word = if methods.is_empty() {
            rtype
        } else {
            let symbol = format!("@\"itab.{}.{}\"", to.go_name(), val.ty.go_name());
            if self.iface_globals.insert(symbol.clone()) {
                let mut slots = vec![format!("ptr {rtype}")];
                slots.extend(fns.iter().map(|f| format!("ptr @\"{f}\"")));
                self.global_defs.push(format!(
                    "{symbol} = private constant [{} x ptr] [{}]",
                    slots.len(),
                    slots.join(", ")
                ));
            }
            symbol
        };
        // A pointer is stored in the data word directly; other values are boxed.
        let data = if matches!(val.ty.under(), Ty::Ptr(_)) {
            val.repr.clone()
        } else {
            let slot = self.heap_alloc(&val.ty);
            self.store(&val, &slot);
            slot
        };
        Ok(self.make_iface(to, &word, &data))
    }

    /// Tests whether dynamic type `rtype` satisfies `target`. For an interface
    /// target the second result is the first word of the converted value.
    fn assert_test(&mut self, rtype: &str, target: &Ty) -> (String, String) {
        match target.under() {
            Ty::Iface(methods) if methods.is_empty() => {
                let ok = self.ins(format!("icmp ne ptr {rtype}, null"));
                (ok, rtype.to_string())
            }
            Ty::Iface(methods) => {
                let methods = methods.clone();
                let desc = self.iface_desc(target, &methods);
                let tab = self.ins(format!("call ptr @goiaba_itab(ptr {desc}, ptr {rtype})"));
                let ok = self.ins(format!("icmp ne ptr {tab}, null"));
                (ok, tab)
            }
            _ => {
                let want = self.rtype_of(target);
                let ok = self.ins(format!("icmp eq ptr {rtype}, {want}"));
                (ok, want)
            }
        }
    }

    /// Reads the value of type `target` out of an interface whose test passed.
    fn assert_value(&mut self, word: &str, data: &str, target: &Ty) -> Val {
        match target.under() {
            Ty::Iface(_) => self.make_iface(target, word, data),
            Ty::Ptr(_) => Val::new(target.clone(), data),
            _ => self.load(data, target),
        }
    }

    /// `x.(T)`. With `comma_ok` the result is the tuple `(T, bool)`.
    fn type_assert(&mut self, ta: &ast::TypeAssertExpr, comma_ok: bool) -> R<Val> {
        let Some(typ) = &ta.typ else {
            return Err("x.(type) is only allowed in a type switch".into());
        };
        let subject = self.expr_m(&ta.expr)?;
        let Ty::Iface(source) = subject.ty.under().clone() else {
            return Err(format!("{} is not an interface", subject.ty));
        };
        let target = self.resolve_type(typ)?;
        let (rtype, data) = self.iface_parts(&subject.repr, !source.is_empty());
        let (ok, word) = self.assert_test(&rtype, &target);

        if !comma_ok {
            let iface_name = subject.ty.go_name();
            let want_name = target.go_name();
            let iface_text = self.intern(iface_name.as_bytes().to_vec());
            let want_text = self.intern(want_name.as_bytes().to_vec());
            let want_iface = matches!(target.under(), Ty::Iface(_)) as i32;
            self.emit(format!(
                "call void @goiaba.assert_ok(i1 {ok}, ptr {rtype}, ptr {iface_text}, i64 {}, ptr {want_text}, i64 {}, i32 {want_iface})",
                iface_name.len(),
                want_name.len()
            ));
            return Ok(self.assert_value(&word, &data, &target));
        }

        // On failure the value is T's zero value, so only read it when ok.
        let yes = self.new_label("assert.ok");
        let end = self.new_label("assert.end");
        let from = self.f.cur.clone();
        self.cond_br(&ok, &yes, &end);
        self.place_label(&yes);
        let val = self.assert_value(&word, &data, &target);
        let yes_end = self.f.cur.clone();
        self.br(&end);
        self.place_label(&end);
        let ll = target.ll();
        let merged = self.ins(format!(
            "phi {ll} [ zeroinitializer, %{from} ], [ {}, %{yes_end} ]",
            val.repr
        ));
        let tuple = Ty::Tuple(vec![target.clone(), Ty::Bool]);
        let tuple_ll = tuple.ll();
        let a = self.ins(format!("insertvalue {tuple_ll} poison, {ll} {merged}, 0"));
        let b = self.ins(format!("insertvalue {tuple_ll} {a}, i1 {ok}, 1"));
        Ok(Val::new(tuple, b))
    }

    fn type_switch(&mut self, stmt: &ast::TypeSwitchStmt) -> R<()> {
        let objs = self.objs;
        self.push_scope();
        let result = (|| {
            if let Some(init) = &stmt.init {
                self.stmt(init)?;
            }
            // `switch x.(type)` or `switch v := x.(type)`.
            let (bind, guard) = match &stmt.assign {
                Stmt::Expr(e) => (None, e.as_ref()),
                Stmt::Assign(key) => {
                    let a = &objs.a_stmts[*key];
                    let (Some(Expr::Ident(name)), Some(rhs)) = (a.lhs.first(), a.rhs.first())
                    else {
                        return Err("invalid type switch guard".to_string());
                    };
                    (Some(objs.idents[*name].name.as_str()), rhs)
                }
                _ => return Err("invalid type switch guard".to_string()),
            };
            let Expr::TypeAssert(ta) = strip_parens(guard) else {
                return Err("invalid type switch guard".to_string());
            };
            let subject = self.expr_m(&ta.expr)?;
            let Ty::Iface(source) = subject.ty.under().clone() else {
                return Err(format!("{} is not an interface", subject.ty));
            };
            let (rtype, data) = self.iface_parts(&subject.repr, !source.is_empty());

            let end = self.new_label("typeswitch.end");
            let mut clauses = Vec::new();
            for s in &stmt.body.list {
                let Stmt::Case(clause) = s else {
                    return Err("unexpected statement in type switch body".to_string());
                };
                clauses.push((clause, self.new_label("typeswitch.case")));
            }

            // For each clause: the single type it binds to, if it has exactly one.
            let mut bound = Vec::new();
            let mut default = None;
            for (clause, label) in &clauses {
                let Some(types) = clause.list.as_ref().filter(|l| !l.is_empty()) else {
                    default = Some(label.clone());
                    bound.push(None);
                    continue;
                };
                let mut single = None;
                for type_expr in types {
                    let is_nil = matches!(strip_parens(type_expr), Expr::Ident(k) if objs.idents[*k].name == "nil");
                    let hit = if is_nil {
                        self.ins(format!("icmp eq ptr {rtype}, null"))
                    } else {
                        let target = self.resolve_type(type_expr)?;
                        let (ok, _) = self.assert_test(&rtype, &target);
                        single = Some(target);
                        ok
                    };
                    let next = self.new_label("typeswitch.next");
                    self.cond_br(&hit, label, &next);
                    self.place_label(&next);
                }
                bound.push(if types.len() == 1 { single } else { None });
            }
            self.br(default.as_deref().unwrap_or(&end));

            for ((clause, label), target) in clauses.iter().zip(bound) {
                self.place_label(label);
                self.push_scope();
                if let Some(name) = bind {
                    let val = match &target {
                        Some(target) => {
                            let (_, word) = self.assert_test(&rtype, target);
                            self.assert_value(&word, &data, target)
                        }
                        None => subject.clone(),
                    };
                    self.declare(name, val.ty.clone(), Some(val));
                }
                self.f.targets.push((end.clone(), None));
                let r = self.block(&clause.body);
                self.f.targets.pop();
                self.pop_scope();
                r?;
                self.br(&end);
            }
            self.place_label(&end);
            Ok(())
        })();
        self.pop_scope();
        result
    }

    // ---- maps -------------------------------------------------------------

    fn map_new(&mut self, key: &Ty, val: &Ty, hint: &str) -> String {
        let key_type = self.rtype_of(key);
        self.ins(format!(
            "call ptr @goiaba_map_new(ptr {key_type}, i64 {}, i64 {hint})",
            sizeof(val)
        ))
    }

    /// `m[k]` as a value: the stored value, or the zero value when the key is
    /// absent. With `comma_ok` the result is the tuple `(V, bool)`.
    fn map_get(&mut self, map: &str, key: &Ty, val: &Ty, index: &Expr, comma_ok: bool) -> R<Val> {
        let k = self.expr_want(index, key)?;
        let key_slot = self.spill(&k);
        let found = self.ins(format!(
            "call ptr @goiaba_map_get(ptr {map}, ptr {key_slot})"
        ));
        let ok = self.ins(format!("icmp ne ptr {found}, null"));
        let yes = self.new_label("map.hit");
        let end = self.new_label("map.end");
        let from = self.f.cur.clone();
        self.cond_br(&ok, &yes, &end);
        self.place_label(&yes);
        let stored = self.load(&found, val);
        self.br(&end);
        self.place_label(&end);
        let ll = val.ll();
        let merged = self.ins(format!(
            "phi {ll} [ zeroinitializer, %{from} ], [ {}, %{yes} ]",
            stored.repr
        ));
        if !comma_ok {
            return Ok(Val::new(val.clone(), merged));
        }
        let tuple = Ty::Tuple(vec![val.clone(), Ty::Bool]);
        let tuple_ll = tuple.ll();
        let a = self.ins(format!("insertvalue {tuple_ll} poison, {ll} {merged}, 0"));
        let b = self.ins(format!("insertvalue {tuple_ll} {a}, i1 {ok}, 1"));
        Ok(Val::new(tuple, b))
    }

    /// The address an assignment writes to. Unlike `loc`, indexing a map here
    /// inserts the key, so `m[k] = v` and `m[k]++` work on absent keys.
    fn lvalue(&mut self, e: &Expr) -> R<(String, Ty)> {
        if let Expr::Index(ix) = strip_parens(e) {
            let base = self.loc(&ix.expr)?;
            let base_ty = match &base {
                Loc::Place(_, ty) => ty.clone(),
                Loc::Value(val) => val.ty.clone(),
            };
            if let Ty::Map(key, val) = base_ty.under() {
                let map = match base {
                    Loc::Place(ptr, ty) => self.load(&ptr, &ty),
                    Loc::Value(val) => val,
                };
                let k = self.expr_want(&ix.index, key)?;
                let key_slot = self.spill(&k);
                let slot = self.ins(format!(
                    "call ptr @goiaba_map_assign(ptr {}, ptr {key_slot})",
                    map.repr
                ));
                return Ok((slot, val.as_ref().clone()));
            }
        }
        match self.loc(e)? {
            Loc::Place(ptr, ty) => Ok((ptr, ty)),
            Loc::Value(_) => Err("cannot assign to this expression".into()),
        }
    }

    /// Widens an integer operand to i64, for indexes and lengths.
    fn widen_i64(&mut self, val: Val) -> R<String> {
        let val = self.materialize(val)?;
        if !matches!(val.ty.under(), Ty::Int { .. }) {
            return Err(format!("expected an integer, found {}", val.ty));
        }
        Ok(self.convert(val, &INT)?.repr)
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
        let hint = matches!(want.under(), Ty::Int { .. }).then(|| want.clone());
        let saved = std::mem::replace(&mut self.shift_hint, hint);
        let val = match strip_parens(e) {
            Expr::CompositeLit(lit) if lit.typ.is_none() => self.composite(lit, Some(want)),
            _ => self.expr(e),
        };
        self.shift_hint = saved;
        self.coerce(val?, want)
    }

    fn loc(&mut self, e: &Expr) -> R<Loc> {
        let result = self.loc_inner(e);
        result.map_err(|err| self.locate(e.pos(self.objs), err))
    }

    fn loc_inner(&mut self, e: &Expr) -> R<Loc> {
        let objs = self.objs;
        match e {
            Expr::Paren(p) => self.loc(&p.expr),
            Expr::Ident(key) => {
                let name = objs.idents[*key].name.as_str();
                match self.lookup(name) {
                    Some(Binding::Var {
                        ptr, ty, indirect, ..
                    }) => {
                        let ptr = if indirect {
                            self.ins(format!("load ptr, ptr {ptr}"))
                        } else {
                            ptr
                        };
                        Ok(Loc::Place(ptr, ty))
                    }
                    Some(Binding::Const(val)) => Ok(Loc::Value(val)),
                    None => match name {
                        "true" => Ok(Loc::Value(Val::bool(true))),
                        "false" => Ok(Loc::Value(Val::bool(false))),
                        "nil" => Ok(Loc::Value(Val::new(Ty::Nil, "null"))),
                        "_" => Err("cannot use _ as a value".into()),
                        _ if self.funcs.contains_key(&self.q(name)) => {
                            let key = self.q(name);
                            Ok(Loc::Value(self.func_value(&key)?))
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
        match val.ty.under() {
            Ty::Ptr(inner) => {
                self.nilcheck(&val.repr);
                Ok(Loc::Place(val.repr, inner.as_ref().clone()))
            }
            _ => Err(format!("cannot dereference {}", val.ty)),
        }
    }

    /// Returns a pointer to the struct at `loc`, following one pointer level.
    fn struct_ptr(&mut self, loc: Loc) -> R<(String, String)> {
        let val = match loc {
            Loc::Place(ptr, ty) => match ty.under() {
                Ty::Struct(name) => return Ok((ptr, name.clone())),
                _ => self.load(&ptr, &ty),
            },
            Loc::Value(val) => val,
        };
        match val.ty.under() {
            Ty::Ptr(inner) => match inner.under() {
                Ty::Struct(name) => {
                    self.nilcheck(&val.repr);
                    Ok((val.repr.clone(), name.clone()))
                }
                _ => Err(format!("{} has no fields", val.ty)),
            },
            Ty::Struct(name) => Ok((self.spill(&val), name.clone())),
            _ => Err(format!("{} has no fields", val.ty)),
        }
    }

    fn field(&mut self, s: &ast::SelectorExpr) -> R<Loc> {
        let sel = self.objs.idents[s.sel].name.as_str();
        if let Some(package) = self.package_of(&s.expr) {
            // `pkg.Name`: a variable, constant or function of another package.
            let key = format!("{package}.{sel}");
            return match self.globals.get(&key).cloned() {
                Some(Binding::Var { ptr, ty, .. }) => Ok(Loc::Place(ptr, ty)),
                Some(Binding::Const(val)) => Ok(Loc::Value(val)),
                None if self.funcs.contains_key(&key) => Ok(Loc::Value(self.func_value(&key)?)),
                None => Err(format!("undefined: {key}")),
            };
        }
        let base = self.loc(&s.expr)?;
        if let Some(value) = self.method_value(&base, sel, &s.expr)? {
            return Ok(Loc::Value(value));
        }
        self.field_of(base, sel)
    }

    /// `x.Method` used as a value: a function value bound to the receiver.
    /// Returns `None` when the type has no such method.
    fn method_value(&mut self, base: &Loc, sel: &str, recv_expr: &Expr) -> R<Option<Val>> {
        let recv_ty = match base {
            Loc::Place(_, ty) => ty.clone(),
            Loc::Value(val) => val.ty.clone(),
        };
        let load = |c: &mut Self, base: &Loc| match base {
            Loc::Place(ptr, ty) => c.load(ptr, ty),
            Loc::Value(val) => val.clone(),
        };
        let bind = |c: &mut Self, sig: FuncTy, code: &str, ctx: &str| {
            let a = c.ins(format!("insertvalue %func poison, ptr {code}, 0"));
            let b = c.ins(format!("insertvalue %func {a}, ptr {ctx}, 1"));
            Val::new(Ty::Func(Box::new(sig)), b)
        };

        // On an interface: the table entry already takes the data word first,
        // which is exactly how a function value passes its context.
        if let Ty::Iface(methods) = recv_ty.under() {
            let Some(index) = methods.iter().position(|(name, _)| name == sel) else {
                return Ok(None);
            };
            let sig = methods[index].1.clone();
            let val = load(self, base);
            let tab = self.ins(format!("extractvalue %iface {}, 0", val.repr));
            let data = self.ins(format!("extractvalue %iface {}, 1", val.repr));
            self.nilcheck(&tab);
            let slot = self.ins(format!(
                "getelementptr inbounds ptr, ptr {tab}, i64 {}",
                index + 1
            ));
            let code = self.ins(format!("load ptr, ptr {slot}"));
            return Ok(Some(bind(self, sig, &code, &data)));
        }

        let (base_ty, recv_is_ptr) = match &recv_ty {
            Ty::Ptr(inner) => (inner.as_ref().clone(), true),
            other => (other.clone(), false),
        };
        let Some(type_name) = base_ty.type_name() else {
            return Ok(None);
        };
        let Some(sig) = self.funcs.get(&format!("{type_name}.{sel}")).cloned() else {
            return Ok(None);
        };
        let code = format!("@\"{}\"", self.iface_fn(&sig));
        // The context is a pointer to the receiver. A value receiver is
        // copied now, as Go evaluates it when the method value is made.
        let ctx = if recv_is_ptr {
            load(self, base).repr
        } else if sig.ptr_recv == Some(true) {
            match base {
                Loc::Place(ptr, _) => {
                    self.mark_escape(recv_expr);
                    ptr.clone()
                }
                Loc::Value(val) => self.spill(val),
            }
        } else {
            let val = load(self, base);
            let copy = self.heap_alloc(&val.ty);
            self.store(&val, &copy);
            copy
        };
        let func_ty = FuncTy {
            params: sig.params[1..].to_vec(),
            ret: sig.ret.clone(),
            variadic: sig.variadic,
        };
        Ok(Some(bind(self, func_ty, &code, &ctx)))
    }

    fn field_of(&mut self, base: Loc, sel: &str) -> R<Loc> {
        let (ptr, struct_name) = self.struct_ptr(base)?;
        let fields = &self.structs[&struct_name];
        let Some(index) = fields.iter().position(|(name, _)| name == sel) else {
            return Err(format!("{struct_name} has no field or method {sel}"));
        };
        let field_ty = fields[index].1.clone();
        let place = self.ins(format!(
            "getelementptr inbounds {}, ptr {ptr}, i32 0, i32 {index}",
            Ty::Struct(struct_name).ll()
        ));
        Ok(Loc::Place(place, field_ty))
    }

    /// The package `e` names, when `e` is an imported package's identifier.
    fn package_of(&mut self, e: &Expr) -> Option<String> {
        let Expr::Ident(key) = strip_parens(e) else {
            return None;
        };
        let name = self.objs.idents[*key].name.as_str();
        if self.lookup(name).is_some() {
            return None;
        }
        self.packages.get(name).cloned()
    }

    fn index(&mut self, ix: &ast::IndexExpr) -> R<Loc> {
        let base = match self.loc(&ix.expr)? {
            Loc::Place(ptr, ty) if matches!(ty.under(), Ty::Array(..)) => {
                Loc::Place(ptr, ty.under().clone())
            }
            other => other,
        };
        let base_ty = match &base {
            Loc::Place(_, ty) => ty.clone(),
            Loc::Value(val) => val.ty.clone(),
        };
        if let Ty::Map(key, val) = base_ty.under() {
            let map = match base {
                Loc::Place(ptr, ty) => self.load(&ptr, &ty),
                Loc::Value(val) => val,
            };
            return Ok(Loc::Value(
                self.map_get(&map.repr, key, val, &ix.index, false)?,
            ));
        }
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
        match val.ty.under() {
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
                Ty::Array(elem, n) => {
                    self.nilcheck(&val.repr);
                    Ok(array_at(self, &val.repr, elem, *n))
                }
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
                    // A typed left operand also types an untyped shift on the right.
                    let typed = matches!(left.ty.under(), Ty::Int { .. })
                        && !matches!(op, Op::Shl | Op::Shr);
                    let saved = self.shift_hint.clone();
                    if typed {
                        self.shift_hint = Some(left.ty.clone());
                    }
                    let right = self.expr(&b.expr_b);
                    self.shift_hint = saved;
                    self.binary(op, left, right?)
                }
            },
            Expr::Unary(u) => self.unary(u),
            Expr::Call(call) => self.call(call),
            Expr::CompositeLit(lit) => self.composite(lit, None),
            Expr::Slice(s) => self.slice_expr(s),
            Expr::FuncLit(lit) => self.func_lit(lit),
            Expr::TypeAssert(ta) => self.type_assert(ta, false),
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
        match (&u.op, val.ty.under(), val.k) {
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
            let ptr = match other.ty.under() {
                Ty::Ptr(_) | Ty::Map(..) => other.repr.clone(),
                Ty::Slice(_) => self.ins(format!("extractvalue %slice {}, 0", other.repr)),
                Ty::Func(_) => self.ins(format!("extractvalue %func {}, 0", other.repr)),
                Ty::Iface(_) => self.ins(format!("extractvalue %iface {}, 0", other.repr)),
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

        // Comparing with an interface compares dynamic types, then values.
        let left_iface = matches!(left.ty.under(), Ty::Iface(_));
        let right_iface = matches!(right.ty.under(), Ty::Iface(_));
        if left_iface || right_iface {
            let (left, right) = if !left_iface {
                let ty = right.ty.clone();
                (self.coerce(left, &ty)?, right)
            } else if !right_iface {
                let ty = left.ty.clone();
                let right = self.coerce(right, &ty)?;
                (left, right)
            } else {
                (left, right)
            };
            let negate = match op {
                Op::Eq => false,
                Op::Ne => true,
                _ => {
                    return Err(format!(
                        "operator {} is not defined on interfaces",
                        op.symbol()
                    ));
                }
            };
            let ty = left.ty.clone();
            let equal = self.value_eq(&ty, &left.repr, &right.repr)?;
            let name = if negate {
                self.ins(format!("xor i1 {equal}, true"))
            } else {
                equal
            };
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

        match ty.under() {
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
            Ty::Struct(_) | Ty::Array(..) if matches!(op, Op::Eq | Op::Ne) => {
                let equal = self.value_eq(&ty, a, b)?;
                let name = if op == Op::Ne {
                    self.ins(format!("xor i1 {equal}, true"))
                } else {
                    equal
                };
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
        // `1 << n` with a variable count: the constant takes the type the
        // context expects, or int when there is none.
        let left = match self.shift_hint.clone() {
            Some(hint) if left.ty.is_untyped_num() => self.coerce(left, &hint)?,
            _ => self.materialize(left)?,
        };
        let Ty::Int { bits, signed } = *left.ty.under() else {
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
        match ty.under() {
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
                let data = self.alloc_array(&n.to_string(), elem);
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
            Ty::Map(key, val) => {
                let (key, val) = (key.as_ref().clone(), val.as_ref().clone());
                let map = self.map_new(&key, &val, &lit.elts.len().to_string());
                for elt in &lit.elts {
                    let Expr::KeyValue(kv) = elt else {
                        return Err("map literal entries must be key: value".into());
                    };
                    let k = self.expr_want(&kv.key, &key)?;
                    let v = self.expr_want(&kv.val, &val)?;
                    let key_slot = self.spill(&k);
                    let slot = self.ins(format!(
                        "call ptr @goiaba_map_assign(ptr {map}, ptr {key_slot})"
                    ));
                    self.store(&v, &slot);
                }
                Ok(Val::new(ty.clone(), map))
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
        let base = match self.loc(&s.expr)? {
            Loc::Place(ptr, ty) if matches!(ty.under(), Ty::Array(..)) => {
                Loc::Place(ptr, ty.under().clone())
            }
            other => other,
        };
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
        match val.ty.under() {
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
                Ty::Array(elem, n) => {
                    self.nilcheck(&val.repr);
                    Ok(from_array(self, &val.repr, elem, *n, high))
                }
                other => Err(format!("cannot slice *{other}")),
            },
            Ty::Array(_, _) => Err("cannot slice an array that is not addressable".into()),
            other => Err(format!("cannot slice {other}")),
        }
    }

    // ---- calls ------------------------------------------------------------

    fn call(&mut self, call: &ast::CallExpr) -> R<Val> {
        self.call_mode(call, false)
    }

    /// Lowers a call. With `capture` the call is evaluated but saved in
    /// `self.f.pending` instead of emitted; only user functions, methods and
    /// function values can be captured.
    fn call_mode(&mut self, call: &ast::CallExpr, capture: bool) -> R<Val> {
        let objs = self.objs;
        match strip_parens(&call.func) {
            Expr::Ident(key) => {
                let name = objs.idents[*key].name.as_str();
                if self.lookup(name).is_some() {
                    let callee = self.expr(&call.func)?;
                    return self.call_value(name, callee, call, capture);
                }
                if let Some(sig) = self.funcs.get(&self.q(name)).cloned() {
                    return self.user_call(name, &sig, None, call, capture);
                }
                match name {
                    "len" | "cap" => self.len_cap(name, call),
                    "append" => self.append(call),
                    "delete" => {
                        let [map, key] = call.args.as_slice() else {
                            return Err("delete takes a map and a key".into());
                        };
                        let map = self.expr(map)?;
                        let Ty::Map(key_ty, _) = map.ty.under().clone() else {
                            return Err(format!("cannot delete from {}", map.ty));
                        };
                        let key = self.expr_want(key, &key_ty)?;
                        let slot = self.spill(&key);
                        self.emit(format!(
                            "call void @goiaba_map_delete(ptr {}, ptr {slot})",
                            map.repr
                        ));
                        Ok(Val::void())
                    }
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
                    "recover" => {
                        let any = Ty::Iface(Vec::new());
                        let out = self.alloca(&any);
                        self.emit(format!("call void @goiaba_recover(ptr {out})"));
                        Ok(self.load(&out, &any))
                    }
                    _ => match self.resolve_type(&call.func) {
                        Ok(ty) => self.conversion(&ty, call),
                        Err(_) => Err(format!("undefined: {name}")),
                    },
                }
            }
            Expr::Selector(s) => self.method_call(s, call, capture),
            Expr::Array(_) | Expr::Star(_) | Expr::Func(_) | Expr::Map(_) | Expr::Interface(_) => {
                let ty = self.resolve_type(&call.func)?;
                self.conversion(&ty, call)
            }
            _ => {
                let callee = self.expr(&call.func)?;
                self.call_value("function value", callee, call, capture)
            }
        }
    }

    /// Calls a function value: `code(ctx, args...)`.
    fn call_value(
        &mut self,
        name: &str,
        callee: Val,
        call: &ast::CallExpr,
        capture: bool,
    ) -> R<Val> {
        let Ty::Func(sig) = callee.ty.under().clone() else {
            return Err(format!("cannot call a value of type {}", callee.ty));
        };
        let code = self.ins(format!("extractvalue %func {}, 0", callee.repr));
        let ctx = self.ins(format!("extractvalue %func {}, 1", callee.repr));
        self.nilcheck(&code);
        let mut args = vec![("ptr".to_string(), code), ("ptr".to_string(), ctx)];
        args.extend(self.lower_args(name, &sig.params, sig.variadic, call)?);
        Ok(self.finish_call(&sig.ret, None, args, capture))
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
        capture: bool,
    ) -> R<Val> {
        let mut args = Vec::new();
        let mut params = sig.params.as_slice();
        if let Some(recv) = receiver {
            params = &params[1..];
            args.push((recv.ty.ll(), recv.repr));
        }
        args.extend(self.lower_args(name, params, sig.variadic, call)?);
        if sig.extern_c {
            let mut scalars = Vec::new();
            for (ty, value) in args {
                let parts: &[&str] = match ty.as_str() {
                    "%string" => &["ptr", "i64"],
                    "%slice" => &["ptr", "i64", "i64"],
                    "%iface" => &["ptr", "ptr"],
                    _ => &[],
                };
                if parts.is_empty() {
                    scalars.push((ty, value));
                    continue;
                }
                for (i, part) in parts.iter().enumerate() {
                    let word = self.ins(format!("extractvalue {ty} {value}, {i}"));
                    scalars.push((part.to_string(), word));
                }
            }
            args = scalars;
        }
        let symbol = format!("@\"{}\"", sig.symbol);
        Ok(self.finish_call(&sig.ret, Some(symbol), args, capture))
    }

    /// Evaluates call arguments against `params` and returns them as typed
    /// LLVM operands. Extra arguments of a variadic call are packed in a slice.
    fn lower_args(
        &mut self,
        name: &str,
        params: &[Ty],
        variadic: bool,
        call: &ast::CallExpr,
    ) -> R<Vec<(String, String)>> {
        let mut args = Vec::new();
        let in_call = |e: String| format!("in call to {name}: {e}");
        let spread = call.ellipsis.is_some();
        if spread && !variadic {
            return Err(format!(
                "{name}: cannot use ... in a call to a non-variadic function"
            ));
        }
        let fixed = if variadic {
            params.len() - 1
        } else {
            params.len()
        };
        let count_ok = if spread {
            call.args.len() == params.len()
        } else if variadic {
            call.args.len() >= fixed
        } else {
            call.args.len() == fixed
        };
        if !count_ok {
            return Err(format!(
                "{name}: expected {} arguments, found {}",
                params.len(),
                call.args.len()
            ));
        }
        for (param, arg) in params[..fixed].iter().zip(&call.args) {
            let val = self.expr_want(arg, param).map_err(in_call)?;
            args.push((param.ll(), val.repr));
        }
        if variadic {
            let slice_ty = &params[fixed];
            let Ty::Slice(elem) = slice_ty.under() else {
                return Err("internal: variadic parameter is not a slice".into());
            };
            let rest = &call.args[fixed..];
            let packed = if spread {
                self.expr_want(&rest[0], slice_ty).map_err(in_call)?
            } else if rest.is_empty() {
                Val::new(slice_ty.clone(), "zeroinitializer")
            } else {
                let n = rest.len().to_string();
                let data = self.alloc_array(&n, elem);
                for (i, arg) in rest.iter().enumerate() {
                    let val = self.expr_want(arg, elem).map_err(in_call)?;
                    let slot = self.ins(format!(
                        "getelementptr inbounds {}, ptr {data}, i64 {i}",
                        elem.ll()
                    ));
                    self.store(&val, &slot);
                }
                self.make_slice(slice_ty.clone(), &data, &n, &n)
            };
            args.push((slice_ty.ll(), packed.repr));
        }
        Ok(args)
    }

    fn method_call(
        &mut self,
        s: &ast::SelectorExpr,
        call: &ast::CallExpr,
        capture: bool,
    ) -> R<Val> {
        let objs = self.objs;
        let method = objs.idents[s.sel].name.as_str();
        if let Some(package) = self.package_of(&s.expr) {
            // `pkg.Func(args)`, or the conversion `pkg.Type(x)`.
            let key = format!("{package}.{method}");
            if let Some(sig) = self.funcs.get(&key).cloned() {
                return self.user_call(&key, &sig, None, call, capture);
            }
            if let Ok(ty) = self.resolve_type(&call.func) {
                return self.conversion(&ty, call);
            }
            return match self.globals.get(&key).cloned() {
                Some(_) => {
                    let callee = self.expr(&call.func)?;
                    self.call_value(&key, callee, call, capture)
                }
                None => Err(format!("undefined: {key}")),
            };
        }

        let recv = self.loc(&s.expr)?;
        let recv_ty = match &recv {
            Loc::Place(_, ty) => ty.clone(),
            Loc::Value(val) => val.ty.clone(),
        };
        // Methods are declared on T; a *T receiver expression reaches them too.
        let (base, recv_is_ptr) = match &recv_ty {
            Ty::Ptr(inner) => (inner.as_ref().clone(), true),
            other => (other.clone(), false),
        };
        if let Ty::Iface(methods) = recv_ty.under() {
            let Some(index) = methods.iter().position(|(name, _)| name == method) else {
                return Err(format!("{recv_ty} has no method {method}"));
            };
            let sig = methods[index].1.clone();
            let val = match recv {
                Loc::Place(ptr, ty) => self.load(&ptr, &ty),
                Loc::Value(val) => val,
            };
            let tab = self.ins(format!("extractvalue %iface {}, 0", val.repr));
            let data = self.ins(format!("extractvalue %iface {}, 1", val.repr));
            self.nilcheck(&tab);
            let slot = self.ins(format!(
                "getelementptr inbounds ptr, ptr {tab}, i64 {}",
                index + 1
            ));
            let code = self.ins(format!("load ptr, ptr {slot}"));
            let mut args = vec![("ptr".to_string(), code), ("ptr".to_string(), data)];
            args.extend(self.lower_args(method, &sig.params, sig.variadic, call)?);
            return Ok(self.finish_call(&sig.ret, None, args, capture));
        }
        let key = base.type_name().map(|name| format!("{name}.{method}"));
        let sig = key.as_ref().and_then(|key| self.funcs.get(key).cloned());
        let (Some(key), Some(sig)) = (key, sig) else {
            // Not a method: a struct field that holds a function value.
            let callee = match self.field_of(recv, method)? {
                Loc::Place(ptr, ty) => self.load(&ptr, &ty),
                Loc::Value(val) => val,
            };
            return self.call_value(method, callee, call, capture);
        };

        // Adjust the receiver to what the method declares, as Go does implicitly.
        let receiver = if sig.ptr_recv == Some(true) {
            match recv {
                Loc::Place(ptr, ty) if !recv_is_ptr => {
                    self.mark_escape(&s.expr);
                    Val::new(Ty::Ptr(Box::new(ty)), ptr)
                }
                Loc::Place(ptr, ty) => self.load(&ptr, &ty),
                Loc::Value(val) if recv_is_ptr => val,
                Loc::Value(val) => {
                    let slot = self.spill(&val);
                    Val::new(Ty::Ptr(Box::new(val.ty)), slot)
                }
            }
        } else {
            let val = match recv {
                Loc::Place(ptr, ty) => self.load(&ptr, &ty),
                Loc::Value(val) => val,
            };
            if recv_is_ptr {
                self.nilcheck(&val.repr);
                self.load(&val.repr, &base)
            } else {
                val
            }
        };
        self.user_call(&key, &sig, Some(receiver), call, capture)
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
        match val.ty.under() {
            Ty::Str if name == "len" => {
                let n = self.ins(format!("extractvalue %string {}, 1", val.repr));
                Ok(Val::new(INT, n))
            }
            Ty::Slice(_) => {
                let index = if name == "len" { 1 } else { 2 };
                let n = self.ins(format!("extractvalue %slice {}, {index}", val.repr));
                Ok(Val::new(INT, n))
            }
            Ty::Map(..) if name == "len" => {
                let n = self.ins(format!("call i64 @goiaba_map_len(ptr {})", val.repr));
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
        if let Ty::Map(key, val) = ty.under() {
            let hint = match call.args.get(1) {
                Some(e) => {
                    let v = self.expr(e)?;
                    self.widen_i64(v)?
                }
                None => "0".to_string(),
            };
            let (key, val) = (key.as_ref().clone(), val.as_ref().clone());
            let map = self.map_new(&key, &val, &hint);
            return Ok(Val::new(ty, map));
        }
        let Ty::Slice(elem) = ty.under() else {
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
        let data = self.alloc_array(&cap, elem);
        Ok(self.make_slice(ty.clone(), &data, &len, &cap))
    }

    fn append(&mut self, call: &ast::CallExpr) -> R<Val> {
        let Some(first) = call.args.first() else {
            return Err("append needs a slice".into());
        };
        let mut slice = self.expr(first)?;
        let Ty::Slice(elem) = slice.ty.under().clone() else {
            return Err(format!(
                "first argument to append must be a slice, found {}",
                slice.ty
            ));
        };
        let size = sizeof(&elem);
        if call.ellipsis.is_some() {
            // append(s, t...): t is a slice of the same element type, or a
            // string when appending to a []byte.
            let [_, rest] = call.args.as_slice() else {
                return Err("append(s, t...) takes exactly two arguments".into());
            };
            let rest = self.expr_m(rest)?;
            let kind = match rest.ty.under() {
                Ty::Slice(other) if *other == elem => "%slice",
                Ty::Str
                    if *elem
                        == (Ty::Int {
                            bits: 8,
                            signed: false,
                        }) =>
                {
                    "%string"
                }
                _ => return Err(format!("cannot append {}... to {}", rest.ty, slice.ty)),
            };
            let data = self.ins(format!("extractvalue {kind} {}, 0", rest.repr));
            let len = self.ins(format!("extractvalue {kind} {}, 1", rest.repr));
            let name = self.ins(format!(
                "call %slice @goiaba.append(%slice {}, ptr {data}, i64 {len}, i64 {size})",
                slice.repr
            ));
            return Ok(Val::new(slice.ty, name));
        }
        for arg in &call.args[1..] {
            let val = self.expr_want(arg, &elem)?;
            let slot = self.spill(&val);
            let name = self.ins(format!(
                "call %slice @goiaba.append(%slice {}, ptr {slot}, i64 1, i64 {size})",
                slice.repr
            ));
            slice = Val::new(slice.ty, name);
        }
        Ok(slice)
    }

    /// Prints one value the way Go's `print` builtin does.
    fn print_val(&mut self, val: &Val) -> R<()> {
        match val.ty.under() {
            Ty::Int { signed, .. } => {
                let signed = *signed;
                let wide = self.convert(val.clone(), &Ty::Int { bits: 64, signed })?;
                let callee = if signed {
                    "goiaba_print_int"
                } else {
                    "goiaba_print_uint"
                };
                self.emit(format!("call void @{callee}(i64 {})", wide.repr));
            }
            Ty::Float(_) => {
                let wide = self.convert(val.clone(), &Ty::Float(64))?;
                self.emit(format!(
                    "call void @goiaba_print_float(double {})",
                    wide.repr
                ));
            }
            Ty::Bool => {
                let wide = self.ins(format!("zext i1 {} to i32", val.repr));
                self.emit(format!("call void @goiaba_print_bool(i32 {wide})"));
            }
            Ty::Str => {
                let data = self.ins(format!("extractvalue %string {}, 0", val.repr));
                let len = self.ins(format!("extractvalue %string {}, 1", val.repr));
                self.emit(format!(
                    "call void @goiaba_print_string(ptr {data}, i64 {len})"
                ));
            }
            Ty::Ptr(_) | Ty::Map(..) => {
                self.emit(format!("call void @goiaba_print_pointer(ptr {})", val.repr));
            }
            Ty::Iface(_) => {
                // Like Go: the two words of the interface value.
                self.print_text("(");
                for (i, sep) in [",", ")"].iter().enumerate() {
                    let word = self.ins(format!("extractvalue %iface {}, {i}", val.repr));
                    self.emit(format!("call void @goiaba_print_pointer(ptr {word})"));
                    self.print_text(sep);
                }
            }
            _ => return Err(format!("cannot print a value of type {}", val.ty)),
        }
        Ok(())
    }

    fn print_text(&mut self, text: &str) {
        let global = self.intern(text.as_bytes().to_vec());
        self.emit(format!(
            "call void @goiaba_print_string(ptr {global}, i64 {})",
            text.len()
        ));
    }

    /// `print` and `println` write to standard error, like the Go builtins.
    fn print(&mut self, args: &[Expr], newline: bool) -> R<Val> {
        // Every argument is evaluated before anything is written.
        let vals = args
            .iter()
            .map(|arg| self.expr_m(arg))
            .collect::<R<Vec<_>>>()?;
        for (i, val) in vals.iter().enumerate() {
            if newline && i > 0 {
                self.print_text(" ");
            }
            self.print_val(val)?;
        }
        if newline {
            self.print_text("\n");
        }
        Ok(Val::void())
    }

    /// `panic(v)`: v becomes an `any` value handed to the runtime.
    fn panic(&mut self, call: &ast::CallExpr) -> R<Val> {
        let [arg] = call.args.as_slice() else {
            return Err("panic takes exactly one argument".into());
        };
        let any = Ty::Iface(Vec::new());
        let val = self.expr_want(arg, &any)?;
        let rtype = self.ins(format!("extractvalue %iface {}, 0", val.repr));
        let data = self.ins(format!("extractvalue %iface {}, 1", val.repr));
        self.emit(format!("call void @goiaba_panic(ptr {rtype}, ptr {data})"));
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
        let result = self.stmt_inner(s);
        result.map_err(|err| self.locate(s.pos(self.objs), err))
    }

    fn stmt_inner(&mut self, s: &Stmt) -> R<()> {
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
                let (ptr, ty) = self.lvalue(&inc.expr)?;
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
            Stmt::Defer(d) => self.defer_stmt(d),
            Stmt::Send(_) | Stmt::Select(_) | Stmt::Comm(_) => {
                Err("channels are not supported yet".into())
            }
            Stmt::TypeSwitch(ts) => self.type_switch(ts),
            Stmt::Case(_) => Err("case clause outside of a switch".into()),
            Stmt::Bad(_) => Err("invalid statement".into()),
        }
    }

    fn local_decl(&mut self, decl: &Decl) -> R<()> {
        let objs = self.objs;
        let Decl::Gen(g) = decl else {
            return Err("unsupported local declaration".into());
        };
        if matches!(g.token, Token::CONST) {
            return self.const_group(&g.specs);
        }
        for key in &g.specs {
            match &objs.specs[*key] {
                Spec::Type(ts) => {
                    if let Expr::Struct(st) = &ts.typ {
                        let name = self.q(&objs.idents[ts.name].name);
                        self.declare_struct_name(&name);
                        let fields = self.struct_fields(st)?;
                        self.structs.insert(name, fields);
                    } else {
                        self.define_type(ts)?;
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
            let val = match strip_parens(&rhs[0]) {
                // `v, ok := x.(T)` reports failure instead of panicking.
                Expr::TypeAssert(ta) if n == 2 => self.type_assert(ta, true)?,
                // `v, ok := m[k]` reports whether the key is present.
                Expr::Index(ix) if n == 2 => {
                    let map = self.expr(&ix.expr)?;
                    let Ty::Map(key, val) = map.ty.under().clone() else {
                        return Err(format!("assignment mismatch: {n} variables but 1 value"));
                    };
                    self.map_get(&map.repr, &key, &val, &ix.index, true)?
                }
                _ => self.expr(&rhs[0])?,
            };
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
            let (ptr, ty) = self.lvalue(lhs)?;
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
            let (ptr, ty) = self.lvalue(&a.lhs[0])?;
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
                let (ptr, ty) = self.lvalue(lhs)?;
                let val = self.coerce(val, &ty)?;
                self.store(&val, &ptr);
            }
        }
        Ok(())
    }

    fn ret(&mut self, ret: &ast::ReturnStmt) -> R<()> {
        let want = self.f.ret.clone();
        let types = match &want {
            Ty::Void => Vec::new(),
            Ty::Tuple(items) => items.clone(),
            other => vec![other.clone()],
        };
        let vals: Vec<Val> = if ret.results.is_empty() && !types.is_empty() {
            // A bare return reads the named results.
            let names = self.f.named_results.clone();
            if names.is_empty() {
                return Err("missing return value".into());
            }
            names
                .iter()
                .map(|name| match self.lookup(name) {
                    Some(Binding::Var { ptr, ty, .. }) => Ok(self.load(&ptr, &ty)),
                    _ => Err(format!("internal: result {name} is not a variable")),
                })
                .collect::<R<_>>()?
        } else if ret.results.len() == types.len() {
            ret.results
                .iter()
                .zip(&types)
                .map(|(e, ty)| self.expr_want(e, ty))
                .collect::<R<_>>()?
        } else if ret.results.len() == 1 && types.len() > 1 {
            // `return f()` where f returns the same tuple.
            let val = self.expr(&ret.results[0])?;
            if val.ty != want {
                return Err(format!("cannot return {} as {want}", val.ty));
            }
            let ll = want.ll();
            types
                .iter()
                .enumerate()
                .map(|(i, ty)| {
                    let name = self.ins(format!("extractvalue {ll} {}, {i}", val.repr));
                    Val::new(ty.clone(), name)
                })
                .collect()
        } else {
            return Err(format!(
                "wrong number of return values: want {}, got {}",
                types.len(),
                ret.results.len()
            ));
        };

        if self.f.uses_defer {
            // Store the results, then leave through the block that runs the defers.
            let slots = self.f.ret_slots.clone();
            for (val, (slot, _)) in vals.iter().zip(&slots) {
                self.emit(format!(
                    "store volatile {} {}, ptr {slot}",
                    val.ty.ll(),
                    val.repr
                ));
            }
            self.br("fn.exit");
            return Ok(());
        }
        self.emit_ret(&want, &vals);
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
        let objs = self.objs;
        self.push_scope();
        let result = (|| {
            if let Some(init) = &stmt.init {
                self.stmt(init)?;
            }
            // Each iteration has its own copy of a loop variable. That is only
            // observable when the variable's address escapes, so only those
            // variables get a slot that is repointed before every post statement.
            let mut per_iteration = Vec::new();
            if let Some(Stmt::Assign(key)) = &stmt.init
                && matches!(objs.a_stmts[*key].token, Token::DEFINE)
            {
                for lhs in &objs.a_stmts[*key].lhs {
                    let Expr::Ident(ident) = lhs else { continue };
                    let name = objs.idents[*ident].name.as_str();
                    let scope = self.f.scopes.last().and_then(|s| s.get(name)).cloned();
                    if let Some(Binding::Var {
                        ptr,
                        ty,
                        heap: true,
                        indirect: false,
                    }) = scope
                    {
                        let slot = self.alloca(&Ty::Nil);
                        self.emit(format!("store ptr {ptr}, ptr {slot}"));
                        self.bind(
                            name,
                            Binding::Var {
                                ptr: slot.clone(),
                                ty: ty.clone(),
                                heap: true,
                                indirect: true,
                            },
                        );
                        per_iteration.push((slot, ty));
                    }
                }
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
            for (slot, ty) in &per_iteration {
                let old = self.ins(format!("load ptr, ptr {slot}"));
                let val = self.load(&old, ty);
                let fresh = self.heap_alloc(ty);
                self.store(&val, &fresh);
                self.emit(format!("store ptr {fresh}, ptr {slot}"));
            }
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
        /// What a range loop walks over.
        enum Subject {
            Count,
            /// First element's address and the element type.
            Elems(String, Ty),
            /// The string's bytes; iteration yields runes.
            Runes(String),
        }
        self.push_scope();
        let result = (|| {
            let subject = self.expr_m(&stmt.expr)?;
            if let Ty::Map(key, val) = subject.ty.under().clone() {
                return self.range_map(stmt, &subject.repr, &key, &val);
            }
            let (len, kind) = match subject.ty.under() {
                Ty::Int { .. } => (self.widen_i64(subject.clone())?, Subject::Count),
                Ty::Slice(elem) => {
                    let elem = elem.as_ref().clone();
                    let data = self.ins(format!("extractvalue %slice {}, 0", subject.repr));
                    let len = self.ins(format!("extractvalue %slice {}, 1", subject.repr));
                    (len, Subject::Elems(data, elem))
                }
                Ty::Array(elem, n) => {
                    let (elem, n) = (elem.as_ref().clone(), *n);
                    let slot = self.spill(&subject);
                    (n.to_string(), Subject::Elems(slot, elem))
                }
                Ty::Str => {
                    let data = self.ins(format!("extractvalue %string {}, 0", subject.repr));
                    let len = self.ins(format!("extractvalue %string {}, 1", subject.repr));
                    (len, Subject::Runes(data))
                }
                _ => return Err(format!("cannot range over {} yet", subject.ty)),
            };
            let define = matches!(stmt.token, Token::DEFINE);
            let counter = self.alloca(&INT);
            self.store(&Val::int(INT, 0), &counter);
            // Bytes consumed by the current rune when ranging over a string.
            let width = self.alloca(&INT);
            self.store(&Val::int(INT, 1), &width);

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
                let item = match &kind {
                    Subject::Count => None,
                    Subject::Elems(data, elem) => {
                        let slot = self.ins(format!(
                            "getelementptr inbounds {}, ptr {data}, i64 {}",
                            elem.ll(),
                            i.repr
                        ));
                        Some(self.load(&slot, elem))
                    }
                    Subject::Runes(data) => {
                        let rune = self.ins(format!(
                            "call i32 @goiaba_decode_rune(ptr {data}, i64 {len}, i64 {}, ptr {width})",
                            i.repr
                        ));
                        Some(Val::new(
                            Ty::Int {
                                bits: 32,
                                signed: true,
                            },
                            rune,
                        ))
                    }
                };
                if let Some(key) = &stmt.key {
                    self.range_bind(key, i.clone(), define)?;
                }
                if let Some(value) = &stmt.val {
                    let Some(item) = item else {
                        return Err("range over an integer yields only one value".to_string());
                    };
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
            let step = self.load(&width, &INT);
            let next = self.ins(format!("add i64 {}, {}", cur.repr, step.repr));
            self.store(&Val::new(INT, next), &counter);
            self.br(&cond);
            self.place_label(&end);
            Ok(())
        })();
        self.pop_scope();
        result
    }

    /// `for k, v := range m`. Entries come in insertion order.
    fn range_map(&mut self, stmt: &ast::RangeStmt, map: &str, key: &Ty, val: &Ty) -> R<()> {
        let define = matches!(stmt.token, Token::DEFINE);
        let cursor = self.alloca(&INT);
        self.store(&Val::int(INT, 0), &cursor);
        let key_out = self.alloca(&Ty::Nil);
        let val_out = self.alloca(&Ty::Nil);

        let cond = self.new_label("range.cond");
        let body = self.new_label("range.body");
        let end = self.new_label("range.end");
        self.place_label(&cond);
        let more = self.ins(format!(
            "call i32 @goiaba_map_next(ptr {map}, ptr {cursor}, ptr {key_out}, ptr {val_out})"
        ));
        let more = self.ins(format!("icmp ne i32 {more}, 0"));
        self.cond_br(&more, &body, &end);

        self.place_label(&body);
        self.push_scope();
        let result = (|| {
            if let Some(target) = &stmt.key {
                let slot = self.ins(format!("load ptr, ptr {key_out}"));
                let k = self.load(&slot, key);
                self.range_bind(target, k, define)?;
            }
            if let Some(target) = &stmt.val {
                let slot = self.ins(format!("load ptr, ptr {val_out}"));
                let v = self.load(&slot, val);
                self.range_bind(target, v, define)?;
            }
            self.f.targets.push((end.clone(), Some(cond.clone())));
            let r = self.block(&stmt.body.list);
            self.f.targets.pop();
            r
        })();
        self.pop_scope();
        result?;
        self.br(&cond);
        self.place_label(&end);
        Ok(())
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
        let (ptr, ty) = self.lvalue(target)?;
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
