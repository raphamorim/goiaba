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
//!
//! This file holds the data types, the package driver and module assembly.
//! The lowering itself is split by concern:
//! - `resolve`: type declarations, structs and signatures;
//! - `func`: function bodies, closures, defer frames, the emitter and scopes;
//! - `stmt`, `expr`, `call`: statements, expressions and calls;
//! - `convert`: implicit and explicit conversions;
//! - `iface`: interfaces and run-time type descriptors;
//! - `literal`: literals and constant folding.

use super::runtime::{c_escape, prelude};
use super::stdlib;
use crate::parser::ast::{self, Decl, Expr, Node, Spec, Stmt};
use crate::parser::objects::AstObjects;
use crate::parser::{ErrorList, FileSet, Parser, Token};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::Write as _;

mod call;
mod convert;
mod expr;
mod func;
mod iface;
mod literal;
mod resolve;
mod stmt;

use literal::{fold, parse_int, unquote_rune, unquote_string};

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
    /// A channel of the given element type; a pointer to a runtime object.
    Chan(Box<Ty>),
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
            Ty::Struct(name) if name.starts_with("struct {") => format!("%\"{name}\""),
            Ty::Struct(name) if name.contains('.') => format!("%\"{name}\""),
            Ty::Struct(name) => format!("%\"main.{name}\""),
            Ty::Named(_, inner) => inner.ll(),
            Ty::Ptr(_) | Ty::Map(..) | Ty::Chan(_) | Ty::Nil => "ptr".into(),
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
            Ty::Struct(name) if name.starts_with("struct {") => name.clone(),
            Ty::Struct(name) | Ty::Named(name, _) if name.contains('.') => name.clone(),
            Ty::Struct(name) | Ty::Named(name, _) => format!("main.{name}"),
            Ty::Ptr(inner) => format!("*{}", inner.go_name()),
            Ty::Slice(elem) => format!("[]{}", elem.go_name()),
            Ty::Map(key, val) => format!("map[{}]{}", key.go_name(), val.go_name()),
            Ty::Chan(elem) => format!("chan {}", elem.go_name()),
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
            Ty::Chan(elem) => write!(f, "chan {elem}"),
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
    /// A type declared inside a function.
    Type(Ty),
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

/// A struct's fields as (name, type), and the indexes of the embedded ones.
type StructLayout = (Vec<(String, Ty)>, Vec<usize>);

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
/// A statement that `break` (and, for loops, `continue`) can leave.
struct Target {
    brk: String,
    cont: Option<String>,
    label: Option<String>,
}

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
    targets: Vec<Target>,
    /// Label of the statement being lowered, with the depth of `targets` at
    /// that statement: the loop, switch or select pushed there takes it.
    pending_label: Option<(String, usize)>,
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
            pending_label: None,
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

/// One input file. Parser positions are offsets from `base`.
struct SourceFile {
    name: String,
    base: usize,
    text: String,
}

/// One parsed package: the main program or an imported library.
struct Package {
    /// Empty for the main package, otherwise the package name.
    name: String,
    files: Vec<SourceFile>,
    objs: AstObjects,
    /// The syntax tree of each file, in the same order as `files`.
    asts: Vec<ast::File>,
}

struct Compiler<'a> {
    objs: &'a AstObjects,
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
    /// Counter that keeps types declared inside functions distinct.
    local_types: usize,
    /// Go global variables, as (symbol, size), for the collector to scan.
    gc_roots: Vec<(String, String)>,
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
    /// Indexes of the embedded fields of each struct.
    embedded: HashMap<String, Vec<usize>>,
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
    /// The Go module being built, so imports of its own packages resolve to
    /// directories. Without it only the bundled standard library is found.
    pub module: Option<Module>,
}

/// A Go module on disk: the path declared in `go.mod` and its directory.
#[derive(Debug, Clone)]
pub struct Module {
    pub path: String,
    pub root: std::path::PathBuf,
}

impl Module {
    /// Finds the `go.mod` at or above `dir` and reads its module path.
    pub fn find(dir: &std::path::Path) -> Option<Module> {
        let start = dir.canonicalize().ok()?;
        for root in start.ancestors() {
            let Ok(text) = std::fs::read_to_string(root.join("go.mod")) else {
                continue;
            };
            let path = text
                .lines()
                .find_map(|line| line.trim().strip_prefix("module "))?
                .trim()
                .trim_matches('"')
                .to_string();
            return Some(Module {
                path,
                root: root.to_path_buf(),
            });
        }
        None
    }

    /// The directory of the package with this import path, if it is part of
    /// the module.
    fn dir_of(&self, import_path: &str) -> Option<std::path::PathBuf> {
        if import_path == self.path {
            return Some(self.root.clone());
        }
        let rest = import_path.strip_prefix(&self.path)?.strip_prefix('/')?;
        Some(self.root.join(rest))
    }
}

/// Reads the Go files of a package directory, skipping tests, in name order.
pub fn read_package_dir(dir: &std::path::Path) -> Result<Vec<(String, String)>, String> {
    let entries = std::fs::read_dir(dir).map_err(|e| format!("{}: {e}", dir.display()))?;
    let mut paths: Vec<_> = entries
        .filter_map(|entry| entry.ok().map(|e| e.path()))
        .filter(|path| {
            let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
            path.is_file() && name.ends_with(".go") && !name.ends_with("_test.go")
        })
        .collect();
    paths.sort();
    if paths.is_empty() {
        return Err(format!("no Go files in {}", dir.display()));
    }
    paths
        .into_iter()
        .map(|path| {
            let text =
                std::fs::read_to_string(&path).map_err(|e| format!("{}: {e}", path.display()))?;
            Ok((path.to_string_lossy().to_string(), text))
        })
        .collect()
}

/// The key a package is stored under. A standard-library package uses its
/// last path element (`unicode/utf8` is `utf8`); a package of the module
/// being built uses its whole path with dots, so two local packages with the
/// same last element stay distinct.
fn package_key(path: &str, module: Option<&Module>) -> String {
    match module {
        Some(m) if m.dir_of(path).is_some() => path.replace('/', "."),
        _ => path.rsplit('/').next().unwrap_or(path).to_string(),
    }
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
    load_imports(&main, opts.module.as_ref(), &mut packages, &mut Vec::new())?;
    packages.push(main);

    let mut compiler = Compiler::new(&packages[0], opts.clone());
    for package in &packages {
        compiler.compile_package(package)?;
    }
    Ok(compiler.finish())
}

/// Parses the files of one package into a shared syntax arena.
fn parse_package(name: &str, files: &[(String, String)]) -> Result<Package, String> {
    let mut objs = AstObjects::new();
    let mut file_set = FileSet::new();
    let mut sources = Vec::new();
    let mut asts = Vec::new();
    for (file_name, text) in files {
        let errors = ErrorList::new();
        let file = file_set.add_file(file_name.clone(), None, text.len());
        let base = file.base();
        let parsed = Parser::new(&mut objs, file, &errors, text, false).parse_file();
        match parsed {
            Some(ast) if errors.is_empty() => asts.push(ast),
            _ => {
                errors.sort();
                return Err(format!("Parse errors: {errors}"));
            }
        }
        sources.push(SourceFile {
            name: file_name.clone(),
            base,
            text: text.clone(),
        });
    }
    Ok(Package {
        name: name.to_string(),
        files: sources,
        objs,
        asts,
    })
}

/// The import paths of a package with the local name each is bound to.
fn imports_of(package: &Package) -> Vec<(String, String)> {
    let objs = &package.objs;
    let mut out = Vec::new();
    for key in package.asts.iter().flat_map(|ast| &ast.imports) {
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
    module: Option<&Module>,
    loaded: &mut Vec<Package>,
    visiting: &mut Vec<String>,
) -> Result<(), String> {
    for (path, _) in imports_of(package) {
        let key = package_key(&path, module);
        if loaded.iter().any(|p| p.name == key) {
            continue;
        }
        if visiting.contains(&key) {
            return Err(format!("import cycle through package {path}"));
        }
        let files = match module.and_then(|m| m.dir_of(&path)) {
            Some(dir) => read_package_dir(&dir)?,
            None => match stdlib::source(&path) {
                Some(text) => vec![(format!("{path}.go"), text.to_string())],
                None => {
                    return Err(format!(
                        "package {path} is not available: goiaba ships a small subset of the standard library and cannot fetch dependencies"
                    ));
                }
            },
        };
        let dependency = parse_package(&key, &files)?;
        visiting.push(key);
        load_imports(&dependency, module, loaded, visiting)?;
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
            files: &first.files,
            opts,
            pkg: String::new(),
            pending_globals: Vec::new(),
            inits: Vec::new(),
            has_main: false,
            local_types: 0,
            gc_roots: Vec::new(),
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
            embedded: HashMap::new(),
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
        let file = self
            .files
            .iter()
            .rev()
            .find(|f| f.base <= pos)
            .or(self.files.first());
        let Some(file) = file else {
            return "?".to_string();
        };
        let offset = (pos - file.base.min(pos)).min(file.text.len());
        let before = &file.text[..offset];
        let line = before.matches('\n').count() + 1;
        let column = before.len() - before.rfind('\n').map_or(0, |i| i + 1) + 1;
        format!("{}:{line}:{column}", file.name)
    }

    /// The name given by an `//export` comment above the function, if any.
    fn export_name(&self, func_name: &str) -> Option<String> {
        self.files
            .iter()
            .find_map(|file| export_name(&file.text, func_name))
    }

    /// Prefixes an error with a source position, unless it already has one.
    fn locate(&mut self, pos: usize, error: String) -> String {
        if self.err_located {
            return error;
        }
        self.err_located = true;
        format!("{}: {error}", self.position(pos))
    }

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
        self.files = &package.files;
        self.pkg = package.name.clone();
        self.err_located = false;
        // Package-level declarations bind into the global scope, so no
        // function state may be left over from the previous package.
        self.f = FnCtx::new(Ty::Void, HashSet::new());
        self.packages.clear();
        for (path, local) in imports_of(package) {
            let key = package_key(&path, self.opts.module.as_ref());
            self.packages.insert(local, key);
        }
        let objs = self.objs;
        let mut type_specs = Vec::new();
        let mut value_decls = Vec::new();
        let mut func_decls = Vec::new();
        for decl in package.asts.iter().flat_map(|ast| &ast.decls) {
            match decl {
                Decl::Gen(g) => match g.token {
                    Token::TYPE => {
                        for key in &g.specs {
                            if let Spec::Type(ts) = &objs.specs[*key] {
                                if ts.typ_params.is_some() {
                                    let pos = objs.idents[ts.name].pos;
                                    let e = "generic types are parsed but not supported by the LLVM backend yet";
                                    return Err(self.locate(pos, e.into()));
                                }
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
                let result = self.define_struct(&name, st);
                result.map_err(|e| self.locate(objs.idents[ts.name].pos, e))?;
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
            } else if let Some(exported) = self.export_name(name) {
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

        self.lower_init(&init_funcs, &func_decls)?;

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
    fn lower_init(&mut self, init_funcs: &[String], funcs: &[&'a ast::FuncDecl]) -> R<()> {
        let objs = self.objs;
        let pending = std::mem::take(&mut self.pending_globals);
        if pending.is_empty() && init_funcs.is_empty() {
            return Ok(());
        }
        let pending = self.order_initializers(pending, funcs);
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

    /// The source text between two parser positions of the current package.
    fn text_between(&self, from: usize, to: usize) -> &'a str {
        let file = self.files.iter().rev().find(|f| f.base <= from);
        let Some(file) = file else {
            return "";
        };
        let start = (from - file.base).min(file.text.len());
        let end = (to.max(from) - file.base).min(file.text.len());
        file.text.get(start..end).unwrap_or("")
    }

    /// Orders package-level initializers so that a variable is initialized
    /// after the variables its initializer uses, directly or through the
    /// functions it calls, as Go requires. Dependencies are found by name in
    /// the source text, which can only add ordering constraints; variables
    /// that do not depend on each other keep their declaration order.
    fn order_initializers(
        &self,
        pending: Vec<(&'a ast::ValueSpec, usize)>,
        funcs: &[&'a ast::FuncDecl],
    ) -> Vec<(&'a ast::ValueSpec, usize)> {
        let objs = self.objs;
        fn words(text: &str) -> impl Iterator<Item = &str> {
            text.split(|c: char| !(c.is_alphanumeric() || c == '_'))
                .filter(|w| !w.is_empty())
        }
        // Which pending initializer defines each variable name.
        let mut owner: HashMap<&str, usize> = HashMap::new();
        for (i, (vs, index)) in pending.iter().enumerate() {
            for (n, name) in vs.names.iter().enumerate() {
                if *index == usize::MAX || *index == n {
                    owner.insert(objs.idents[*name].name.as_str(), i);
                }
            }
        }
        let mut bodies: HashMap<&str, Vec<&str>> = HashMap::new();
        for fd in funcs {
            if let Some(body) = &fd.body {
                let text = self.text_between(body.l_brace, body.r_brace);
                bodies
                    .entry(objs.idents[fd.name].name.as_str())
                    .or_default()
                    .push(text);
            }
        }
        // The variables each initializer reaches, following function bodies.
        let deps: Vec<Vec<usize>> = pending
            .iter()
            .enumerate()
            .map(|(i, (vs, index))| {
                let exprs: Vec<&Expr> = if *index == usize::MAX {
                    vs.values.iter().collect()
                } else {
                    vec![&vs.values[*index]]
                };
                let mut queue: Vec<&str> = exprs
                    .iter()
                    .map(|e| self.text_between(e.pos(objs), e.end(objs)))
                    .collect();
                let mut seen_funcs = HashSet::new();
                let mut found = Vec::new();
                while let Some(text) = queue.pop() {
                    for word in words(text) {
                        if let Some(&j) = owner.get(word)
                            && j != i
                            && !found.contains(&j)
                        {
                            found.push(j);
                        }
                        if let Some(texts) = bodies.get(word)
                            && seen_funcs.insert(word)
                        {
                            queue.extend(texts.iter().copied());
                        }
                    }
                }
                found.sort_unstable();
                found
            })
            .collect();

        // Depth-first: dependencies first, otherwise declaration order.
        fn visit(i: usize, deps: &[Vec<usize>], state: &mut [u8], order: &mut Vec<usize>) {
            if state[i] != 0 {
                return; // done, or part of a cycle being visited
            }
            state[i] = 1;
            for &j in &deps[i] {
                visit(j, deps, state, order);
            }
            state[i] = 2;
            order.push(i);
        }
        let mut state = vec![0u8; pending.len()];
        let mut order = Vec::new();
        for i in 0..pending.len() {
            visit(i, &deps, &mut state, &mut order);
        }
        order.into_iter().map(|i| pending[i]).collect()
    }

    /// Emits a package-level variable and binds its name.
    fn define_global(&mut self, name: &str, ty: &Ty, init: &str) -> String {
        let key = self.q(name);
        let symbol = format!("@\"{}\"", self.symbol_for(&key));
        self.global_defs
            .push(format!("{symbol} = internal global {} {init}", ty.ll()));
        self.gc_roots.push((symbol.clone(), sizeof(ty)));
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
        // The collector's root table: every Go global variable.
        let roots: Vec<String> = self
            .gc_roots
            .iter()
            .map(|(symbol, size)| format!("{{ ptr, i64 }} {{ ptr {symbol}, i64 {size} }}"))
            .collect();
        let _ = writeln!(
            out,
            "@goiaba_roots = constant [{} x {{ ptr, i64 }}] [{}]\n@goiaba_nroots = constant i64 {}",
            roots.len(),
            roots.join(", "),
            roots.len()
        );
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
        let exported =
            self.pkg.is_empty() && shape.ptr_recv.is_none() && self.export_name(name).is_some();
        let def = self.lower_function(&symbol, &shape.sig, &body.list, exported, false);
        let def = def.map_err(|e| self.locate(pos, e))?;
        let nested = std::mem::take(&mut self.f.nested_defs);
        self.fn_defs.extend(nested);
        self.fn_defs.push(def);
        Ok(())
    }
}
