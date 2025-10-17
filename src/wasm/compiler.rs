// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

use crate::parser::FuncTypeKey;
use crate::parser::Token;
use crate::parser::ast::{Spec, TypeSpec};
use crate::parser::parse_str;
use std::collections::HashMap;
use std::rc::Rc;
use wasm_encoder::{
    CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection, ImportSection,
    Instruction, MemorySection, MemoryType, Module as WasmModule, TypeSection, ValType,
};

// Import your internal parser types
use crate::parser::{
    ast::{Decl, Expr, File, FuncDecl, Stmt},
    objects::AstObjects,
};

fn extract_export_name(source: &str, func_name: &str) -> Option<String> {
    let lines: Vec<&str> = source.lines().collect();

    // Find the function definition line
    for (i, line) in lines.iter().enumerate() {
        if line.contains(&format!("func {}", func_name)) {
            // Look backwards from the function definition for export comment
            for j in (0..i).rev() {
                let check_line = lines[j].trim();
                if check_line.starts_with("//export ") {
                    let export_name = check_line.strip_prefix("//export ").unwrap().trim();
                    if !export_name.is_empty() {
                        return Some(export_name.to_string());
                    }
                }
                // Stop if we hit a non-comment, non-empty line
                if !check_line.is_empty() && !check_line.starts_with("//") {
                    break;
                }
            }
            break;
        }
    }
    None
}

// Define our WASM-specific AST structures
#[derive(Debug, Clone)]
pub enum WasmExpr {
    Integer(i32),
    String(String), // String literal
    Variable(String),
    Binary(WasmBinaryOp, Box<WasmExpr>, Box<WasmExpr>),
    Unary(WasmUnaryOp, Box<WasmExpr>),
    Call(String, Vec<WasmExpr>),
    Assign(String, Box<WasmExpr>),
    StructLiteral(String, Vec<(String, WasmExpr)>), // struct_name, field_values
    FieldAccess(Box<WasmExpr>, String),             // object.field
    FieldAssign(Box<WasmExpr>, String, Box<WasmExpr>), // object.field = value
    AddressOf(Box<WasmExpr>),                       // &expr
    Dereference(Box<WasmExpr>),                     // *expr
    ArrayLiteral(Vec<WasmExpr>),                    // [expr1, expr2, ...]
    IndexAccess(Box<WasmExpr>, Box<WasmExpr>),      // array[index]
    IndexAssign(Box<WasmExpr>, Box<WasmExpr>, Box<WasmExpr>), // array[index] = value
    MakeSlice(usize),                               // make([]int, size)
    StringLen(Box<WasmExpr>),                       // len(string)
    StringConcat(Box<WasmExpr>, Box<WasmExpr>),     // string + string
}

#[derive(Debug, Clone)]
pub enum WasmUnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WasmBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem, // Modulo/remainder
    Lt,
    Gt,
    LtEq,
    GtEq,
    Eq,
    NotEq,
    And,        // Bitwise AND
    Or,         // Bitwise OR
    Xor,        // Bitwise XOR
    Shl,        // Left shift
    Shr,        // Right shift
    LogicalOr,  // Logical OR (||)
    LogicalAnd, // Logical AND (&&)
}

#[derive(Debug, Clone)]
pub enum WasmStatement {
    ExprStmt(WasmExpr),
    VarDecl(String, WasmExpr),
    Return(Option<WasmExpr>),
    Block(Vec<WasmStatement>),
    If(WasmExpr, Vec<WasmStatement>, Option<Vec<WasmStatement>>),
    Loop(Vec<WasmStatement>),
    Break,
    Switch(WasmExpr, Vec<(Option<Vec<WasmExpr>>, Vec<WasmStatement>)>), // expr, cases: (case_values, body)
    StructDecl(String, Vec<(String, WasmType)>),                        // struct_name, fields
}

#[derive(Debug, Clone)]
pub struct WasmFunctionDef {
    pub name: String,
    pub export_name: Option<String>,
    pub params: Vec<(String, WasmType)>,
    pub return_type: Option<WasmType>,
    pub body: Vec<WasmStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum WasmType {
    Int,
    Int8,
    Int16,
    Int64,
    Uint,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float,
    Float64,
    Bool,
    Void,
    String,                      // String type
    Struct(String),              // Named struct type
    Pointer(Box<WasmType>),      // Pointer to type
    Array(Box<WasmType>, usize), // Array of type with fixed size
    Slice(Box<WasmType>),        // Slice of type (dynamic)
}

// Struct definition storage
#[derive(Debug, Clone)]
pub struct WasmStructDef {
    pub name: String,
    pub fields: Vec<(String, WasmType)>,
    pub field_offsets: HashMap<String, u32>, // Field name -> byte offset
    pub size: u32,                           // Total struct size in bytes
}

#[derive(Debug)]
pub struct WasmProgram {
    pub functions: Vec<WasmFunctionDef>,
    pub structs: Vec<WasmStructDef>,
    pub imports: Vec<WasmImport>,
    pub imported_packages: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct WasmImport {
    pub module: String,
    pub name: String,
    pub params: Vec<ValType>,
    pub results: Vec<ValType>,
}

// Translator from Go AST to WASM AST
#[derive(Default)]
pub struct GoToWasmTranslator {
    struct_defs: HashMap<String, WasmStructDef>,
    imported_packages: Vec<String>, // List of imported package names
}

impl GoToWasmTranslator {
    pub fn new() -> Self {
        Self {
            struct_defs: HashMap::new(),
            imported_packages: Vec::new(),
        }
    }

    pub fn translate_program(go_program: &File, objs: &AstObjects, source: &str) -> WasmProgram {
        let mut translator = Self::new();
        let mut functions = Vec::new();
        let mut structs = Vec::new();
        let imports = Vec::new();

        // First pass: process imports
        for import_key in &go_program.imports {
            if let Spec::Import(import_spec) = &objs.specs[*import_key] {
                translator.translate_import(import_spec, objs);
            }
        }

        // Second pass: collect struct definitions
        for decl in &go_program.decls {
            if let Decl::Gen(gen_decl) = decl {
                for spec_key in &gen_decl.specs {
                    if let Spec::Type(type_spec) = &objs.specs[*spec_key]
                        && let Some(struct_def) =
                            translator.translate_struct_definition(type_spec, objs)
                    {
                        translator
                            .struct_defs
                            .insert(struct_def.name.clone(), struct_def.clone());
                        structs.push(struct_def);
                    }
                }
            }
        }

        // Third pass: translate functions with struct context
        for decl in &go_program.decls {
            if let Decl::Func(func_decl_key) = decl {
                let func_decl = &objs.fdecls[*func_decl_key];
                functions.push(translator.translate_function(func_decl, objs, source));
            }
        }

        WasmProgram {
            functions,
            structs,
            imports,
            imported_packages: translator.imported_packages.clone(),
        }
    }

    fn extract_function_name(&self, expr: &Expr, objs: &AstObjects) -> String {
        match expr {
            Expr::Ident(func_ident) => objs.idents[*func_ident].name.clone(),
            Expr::Selector(selector_expr) => {
                // This is a qualified name like pkg.Function
                if let Expr::Ident(pkg_ident) = &selector_expr.expr {
                    let pkg_name = objs.idents[*pkg_ident].name.clone();
                    let func_name = objs.idents[selector_expr.sel].name.clone();
                    format!("{}.{}", pkg_name, func_name)
                } else {
                    "unknown".to_string()
                }
            }
            _ => "unknown".to_string(),
        }
    }

    fn translate_import(
        &mut self,
        import_spec: &Rc<crate::parser::ast::ImportSpec>,
        _objs: &AstObjects,
    ) {
        use crate::wasm::std;

        // Extract the import path
        let import_path = match &import_spec.path.token {
            crate::parser::Token::STRING(_) => {
                // Remove quotes from the path
                let path_str = import_spec.path.token.get_literal().trim_matches('"');
                path_str.to_string()
            }
            _ => return, // Invalid import path
        };

        // For now, we only support stdlib packages
        // In the future, this could be extended to support custom packages
        if !std::is_supported_package(&import_path) {
            // For unsupported packages, we'll emit a warning but continue
            // In a real implementation, you might want to error here
            eprintln!(
                "Warning: Package '{}' is not supported, import ignored",
                import_path
            );
            return;
        }

        // Record the imported package
        self.imported_packages.push(import_path);
    }

    fn translate_struct_definition(
        &mut self,
        type_spec: &TypeSpec,
        objs: &AstObjects,
    ) -> Option<WasmStructDef> {
        let struct_name = objs.idents[type_spec.name].name.clone();

        if let Expr::Struct(struct_type) = &type_spec.typ {
            let mut fields = Vec::new();
            let mut field_offsets = HashMap::new();
            let mut current_offset = 0u32;

            for field_key in &struct_type.fields.list {
                let field = &objs.fields[*field_key];
                let field_type = Self::go_type_to_wasm_type(&field.typ, objs);
                let field_size = self.get_type_size_with_context(&field_type);

                // Handle named fields
                if !field.names.is_empty() {
                    for name_key in &field.names {
                        let field_name = objs.idents[*name_key].name.clone();
                        field_offsets.insert(field_name.clone(), current_offset);
                        fields.push((field_name, field_type.clone()));
                        current_offset += field_size;
                    }
                } else {
                    // Anonymous field - generate a name based on type
                    let field_name = format!("field_{}", fields.len());
                    field_offsets.insert(field_name.clone(), current_offset);
                    fields.push((field_name, field_type.clone()));
                    current_offset += field_size;
                }
            }

            return Some(WasmStructDef {
                name: struct_name,
                fields,
                field_offsets,
                size: current_offset,
            });
        }

        None
    }

    // Get type size with access to struct definitions
    fn get_type_size_with_context(&self, wasm_type: &WasmType) -> u32 {
        match wasm_type {
            WasmType::Struct(struct_name) => {
                // Look up the struct definition
                if let Some(struct_def) = self.struct_defs.get(struct_name) {
                    struct_def.size
                } else {
                    16 // Default struct size if not found
                }
            }
            _ => Self::get_type_size(wasm_type),
        }
    }

    fn get_type_size(wasm_type: &WasmType) -> u32 {
        match wasm_type {
            WasmType::Int | WasmType::Uint | WasmType::Uint32 => 4,
            WasmType::Int8 | WasmType::Uint8 | WasmType::Bool => 1,
            WasmType::Int16 | WasmType::Uint16 => 2,
            WasmType::Int64 | WasmType::Uint64 => 8,
            WasmType::Float => 4,   // 32-bit floats
            WasmType::Float64 => 8, // 64-bit floats
            WasmType::Void => 0,
            WasmType::String => 8,     // String as (pointer, length) pair
            WasmType::Pointer(_) => 4, // 32-bit pointers (address)
            WasmType::Struct(_struct_name) => {
                // For now, return a default size - this would need proper struct resolution
                // In a complete implementation, you'd look up the struct definition
                16 // Default struct size
            }
            WasmType::Array(element_type, size) => {
                // Array size = element_size * length
                Self::get_type_size(element_type) * (*size as u32)
            }
            WasmType::Slice(_) => {
                // Slices are represented as (pointer, length) pair
                8 // 4 bytes for pointer + 4 bytes for length
            }
        }
    }

    // Updated translate_function with proper signature extraction
    fn translate_function(
        &self,
        go_func: &FuncDecl,
        objs: &AstObjects,
        source: &str,
    ) -> WasmFunctionDef {
        // Extract function name - resolve IdentKey to get the actual name
        let func_name = objs.idents[go_func.name].name.clone();

        // Extract function signature from the type
        let (params, return_type) = Self::extract_function_signature(&go_func.typ, objs);

        // Extract function body using filtered translation
        let body = if let Some(ref body_block) = go_func.body {
            self.translate_statements(&body_block.list, objs)
        } else {
            Vec::new()
        };

        // Extract export name from comments
        let export_name = extract_export_name(source, &func_name);

        WasmFunctionDef {
            name: func_name,
            export_name,
            params,
            return_type,
            body,
        }
    }

    // Helper function to extract function parameters and return type
    fn extract_function_signature(
        func_type_key: &FuncTypeKey,
        objs: &AstObjects,
    ) -> (Vec<(String, WasmType)>, Option<WasmType>) {
        let func_type = &objs.ftypes[*func_type_key];

        // Extract parameters
        let mut params = Vec::new();
        for field_key in &func_type.params.list {
            let field = &objs.fields[*field_key];

            // Get the parameter type
            let param_type = Self::go_type_to_wasm_type(&field.typ, objs);

            // If field has names, use them; otherwise generate parameter names
            if !field.names.is_empty() {
                for name_key in &field.names {
                    let param_name = objs.idents[*name_key].name.clone();
                    params.push((param_name, param_type.clone()));
                }
            } else {
                // Anonymous parameter - generate a name
                params.push((format!("param_{}", params.len()), param_type));
            }
        }

        // Extract return type
        let return_type = if let Some(ref results) = func_type.results {
            if !results.list.is_empty() {
                let return_field = &objs.fields[results.list[0]];
                Some(Self::go_type_to_wasm_type(&return_field.typ, objs))
            } else {
                None
            }
        } else {
            None
        };

        (params, return_type)
    }

    // Helper function to convert Go types to WASM types
    fn go_type_to_wasm_type(go_type: &Expr, objs: &AstObjects) -> WasmType {
        match go_type {
            Expr::Ident(ident_key) => {
                let type_name = &objs.idents[*ident_key].name;
                match type_name.as_str() {
                    // Integer types
                    "int" => WasmType::Int,
                    "int8" => WasmType::Int8,
                    "int16" => WasmType::Int16,
                    "int32" => WasmType::Int,
                    "int64" => WasmType::Int64,
                    // Unsigned integer types
                    "uint" => WasmType::Uint,
                    "uint8" | "byte" => WasmType::Uint8,
                    "uint16" => WasmType::Uint16,
                    "uint32" => WasmType::Uint32,
                    "uint64" => WasmType::Uint64,
                    // Floating point types
                    "float32" => WasmType::Float,
                    "float64" => WasmType::Float64,
                    // Boolean type
                    "bool" => WasmType::Bool,
                    // String type
                    "string" => WasmType::String,
                    // Other types might be structs
                    other => WasmType::Struct(other.to_string()),
                }
            }
            Expr::Array(array_type) => {
                // Handle array types
                let element_type = Self::go_type_to_wasm_type(&array_type.elt, objs);
                if let Some(len_expr) = &array_type.len &&
                    // Fixed-size array - try to extract the length
                    let Expr::BasicLit(lit) = len_expr
                {
                    let literal = lit.token.get_literal();
                    if let Ok(size) = literal.parse::<usize>() {
                        return WasmType::Array(Box::new(element_type), size);
                    }
                }
                // Dynamic slice
                WasmType::Slice(Box::new(element_type))
            }
            Expr::Star(star_expr) => {
                // Pointer type
                let inner_type = Self::go_type_to_wasm_type(&star_expr.expr, objs);
                WasmType::Pointer(Box::new(inner_type))
            }
            // Handle other type expressions as needed
            _ => WasmType::Int, // Default fallback
        }
    }

    // Helper to translate binary operators using pattern matching
    fn translate_binary_op(go_op: &Token) -> WasmBinaryOp {
        match go_op {
            Token::ADD => WasmBinaryOp::Add,
            Token::SUB => WasmBinaryOp::Sub,
            Token::MUL => WasmBinaryOp::Mul,
            Token::QUO => WasmBinaryOp::Div,
            Token::REM => WasmBinaryOp::Rem, // %
            Token::LSS => WasmBinaryOp::Lt,
            Token::GTR => WasmBinaryOp::Gt,
            Token::LEQ => WasmBinaryOp::LtEq,
            Token::GEQ => WasmBinaryOp::GtEq,
            Token::EQL => WasmBinaryOp::Eq,
            Token::NEQ => WasmBinaryOp::NotEq,
            Token::AND => WasmBinaryOp::And,         // &
            Token::OR => WasmBinaryOp::Or,           // |
            Token::XOR => WasmBinaryOp::Xor,         // ^
            Token::SHL => WasmBinaryOp::Shl,         // <<
            Token::SHR => WasmBinaryOp::Shr,         // >>
            Token::LOR => WasmBinaryOp::LogicalOr,   // ||
            Token::LAND => WasmBinaryOp::LogicalAnd, // &&
            _ => {
                // Handle string-based operators as fallback
                let op_str = format!("{:?}", go_op);
                match op_str.as_str() {
                    "%" => WasmBinaryOp::Rem,
                    "&" => WasmBinaryOp::And,
                    "|" => WasmBinaryOp::Or,
                    "^" => WasmBinaryOp::Xor,
                    "<<" => WasmBinaryOp::Shl,
                    ">>" => WasmBinaryOp::Shr,
                    "||" => WasmBinaryOp::LogicalOr,
                    "&&" => WasmBinaryOp::LogicalAnd,
                    _ => {
                        println!("Warning: Unknown binary operator: {:?}", go_op);
                        WasmBinaryOp::Add // Default fallback
                    }
                }
            }
        }
    }

    // Helper to translate unary operators using pattern matching
    fn translate_unary_op(go_op: &Token) -> WasmUnaryOp {
        match go_op {
            Token::SUB => WasmUnaryOp::Neg,
            Token::NOT => WasmUnaryOp::Not,
            _ => {
                println!("Warning: Unknown unary operator: {:?}", go_op);
                WasmUnaryOp::Neg // Default fallback
            }
        }
    }

    // Helper to process escape sequences in strings
    fn process_escape_sequences(s: &str) -> String {
        let mut result = String::new();
        let mut chars = s.chars();

        while let Some(ch) = chars.next() {
            if ch == '\\' {
                if let Some(next_ch) = chars.next() {
                    match next_ch {
                        'n' => result.push('\n'),
                        't' => result.push('\t'),
                        'r' => result.push('\r'),
                        '\\' => result.push('\\'),
                        '"' => result.push('"'),
                        '\'' => result.push('\''),
                        '0' => result.push('\0'),
                        _ => {
                            // Unknown escape, keep as is
                            result.push('\\');
                            result.push(next_ch);
                        }
                    }
                } else {
                    result.push('\\');
                }
            } else {
                result.push(ch);
            }
        }

        result
    }

    // Helper function to filter and translate statements
    fn translate_statements(&self, go_stmts: &[Stmt], objs: &AstObjects) -> Vec<WasmStatement> {
        go_stmts
            .iter()
            .filter_map(|stmt| self.translate_statement_optional(stmt, objs))
            .collect()
    }

    // Helper to check if an expression is a string type
    fn is_string_expr(&self, expr: &WasmExpr) -> bool {
        // Only String literals and StringConcat produce strings
        // StringLen produces an integer, not a string
        matches!(expr, WasmExpr::String(_) | WasmExpr::StringConcat(_, _))
    }

    fn translate_expression(&self, go_expr: &Expr, objs: &AstObjects) -> WasmExpr {
        match go_expr {
            Expr::BasicLit(lit) => match &lit.token {
                Token::INT(lit_val) => {
                    let value_str: &String = lit_val.as_ref();
                    WasmExpr::Integer(value_str.parse::<i32>().unwrap_or(0))
                }
                Token::CHAR(lit_val) => {
                    let value_str: &String = lit_val.as_ref();
                    WasmExpr::Integer(value_str.chars().next().unwrap_or('0') as i32)
                }
                Token::STRING(lit_val) => {
                    let value_str: &String = lit_val.as_ref();
                    // Remove surrounding quotes if present
                    let cleaned = value_str.trim_matches('"');
                    // Process escape sequences
                    let processed = Self::process_escape_sequences(cleaned);
                    WasmExpr::String(processed)
                }
                _ => WasmExpr::Integer(0),
            },
            Expr::Ident(ident_key) => {
                let ident_name = objs.idents[*ident_key].name.clone();
                WasmExpr::Variable(ident_name)
            }
            Expr::Paren(paren_expr) => self.translate_expression(&paren_expr.expr, objs),
            Expr::Binary(binary_expr) => {
                let left = self.translate_expression(&binary_expr.expr_a, objs);
                let right = self.translate_expression(&binary_expr.expr_b, objs);
                let op = Self::translate_binary_op(&binary_expr.op);

                // Special handling for string concatenation
                if op == WasmBinaryOp::Add && self.is_string_expr(&left) {
                    WasmExpr::StringConcat(Box::new(left), Box::new(right))
                } else {
                    WasmExpr::Binary(op, Box::new(left), Box::new(right))
                }
            }
            Expr::Call(call_expr) => {
                let func_name = self.extract_function_name(&call_expr.func, objs);

                // Handle type conversions (casting)
                // In Go, type conversions look like function calls: int8(x), uint32(y), etc.
                let type_names = [
                    "int", "int8", "int16", "int32", "int64", "uint", "uint8", "uint16", "uint32",
                    "uint64", "byte", "bool", "float32", "float64", "string",
                ];

                if type_names.contains(&func_name.as_str()) && !call_expr.args.is_empty() {
                    // For now, type conversions just pass through the value
                    // A full implementation would handle size conversions, sign extensions, etc.
                    return self.translate_expression(&call_expr.args[0], objs);
                }

                // Handle built-in functions
                if func_name == "len" && !call_expr.args.is_empty() {
                    let arg = self.translate_expression(&call_expr.args[0], objs);
                    return WasmExpr::StringLen(Box::new(arg));
                }

                let args = call_expr
                    .args
                    .iter()
                    .map(|arg| self.translate_expression(arg, objs))
                    .collect();
                WasmExpr::Call(func_name, args)
            }
            Expr::Unary(unary_expr) => {
                let operand = self.translate_expression(&unary_expr.expr, objs);
                let op = Self::translate_unary_op(&unary_expr.op);

                // Handle address-of and dereference operators
                match unary_expr.op {
                    Token::AND => WasmExpr::AddressOf(Box::new(operand)),
                    Token::MUL => WasmExpr::Dereference(Box::new(operand)),
                    _ => WasmExpr::Unary(op, Box::new(operand)),
                }
            }
            // Enhanced struct field access
            Expr::Selector(selector_expr) => {
                let object = self.translate_expression(&selector_expr.expr, objs);
                let field_name = objs.idents[selector_expr.sel].name.clone();
                WasmExpr::FieldAccess(Box::new(object), field_name)
            }
            // Enhanced composite literal support for structs and arrays
            Expr::CompositeLit(composite_lit) => {
                // Check if this is an array/slice literal or struct literal
                let is_array = if let Some(ref typ) = composite_lit.typ {
                    matches!(typ, Expr::Array(_))
                } else {
                    // No type specified, check if elements are key-value pairs
                    !composite_lit.elts.is_empty()
                        && !matches!(composite_lit.elts[0], Expr::KeyValue(_))
                };

                if is_array {
                    // Array literal: []int{1, 2, 3}
                    let elements: Vec<WasmExpr> = composite_lit
                        .elts
                        .iter()
                        .map(|e| self.translate_expression(e, objs))
                        .collect();
                    WasmExpr::ArrayLiteral(elements)
                } else {
                    // Struct literal
                    let struct_name = if let Some(Expr::Ident(type_ident)) = &composite_lit.typ {
                        objs.idents[*type_ident].name.clone()
                    } else {
                        "unknown".to_string()
                    };

                    // Parse field assignments from the element list
                    let mut field_values = Vec::new();

                    for element in &composite_lit.elts {
                        match element {
                            // Key-value pairs: field: value
                            Expr::KeyValue(kv_expr) => {
                                if let Expr::Ident(key_ident) = &kv_expr.key {
                                    let field_name = objs.idents[*key_ident].name.clone();
                                    let field_value = self.translate_expression(&kv_expr.val, objs);
                                    field_values.push((field_name, field_value));
                                }
                            }
                            // Direct values (positional initialization)
                            _ => {
                                let field_name = format!("field_{}", field_values.len());
                                let field_value = self.translate_expression(element, objs);
                                field_values.push((field_name, field_value));
                            }
                        }
                    }

                    WasmExpr::StructLiteral(struct_name, field_values)
                }
            }
            // Array/slice indexing: arr[i]
            Expr::Index(index_expr) => {
                let array = self.translate_expression(&index_expr.expr, objs);
                let index = self.translate_expression(&index_expr.index, objs);
                WasmExpr::IndexAccess(Box::new(array), Box::new(index))
            }
            _ => {
                println!(
                    "Warning: Unsupported expression type for struct support: {:?}",
                    go_expr
                );
                WasmExpr::Integer(0)
            }
        }
    }

    // Improve the assignment handling to deal with complex left-hand sides
    fn translate_statement_optional(
        &self,
        go_stmt: &Stmt,
        objs: &AstObjects,
    ) -> Option<WasmStatement> {
        match go_stmt {
            Stmt::Empty(_) => None,
            Stmt::Expr(expr) => Some(WasmStatement::ExprStmt(
                self.translate_expression(expr, objs),
            )),
            Stmt::Assign(assign_key) => {
                let assign_stmt = &objs.a_stmts[*assign_key];

                if !assign_stmt.lhs.is_empty() && !assign_stmt.rhs.is_empty() {
                    // Handle array index assignments: arr[i] = value
                    if let Expr::Index(index_expr) = &assign_stmt.lhs[0] {
                        let array = self.translate_expression(&index_expr.expr, objs);
                        let index = self.translate_expression(&index_expr.index, objs);
                        let value = self.translate_expression(&assign_stmt.rhs[0], objs);

                        return Some(WasmStatement::ExprStmt(WasmExpr::IndexAssign(
                            Box::new(array),
                            Box::new(index),
                            Box::new(value),
                        )));
                    }

                    // Handle field assignments
                    if let Expr::Selector(selector_expr) = &assign_stmt.lhs[0] {
                        let object = self.translate_expression(&selector_expr.expr, objs);
                        let field_name = objs.idents[selector_expr.sel].name.clone();
                        let value = self.translate_expression(&assign_stmt.rhs[0], objs);

                        return Some(WasmStatement::ExprStmt(WasmExpr::FieldAssign(
                            Box::new(object),
                            field_name,
                            Box::new(value),
                        )));
                    }

                    // Handle simple variable assignment
                    if let Expr::Ident(var_key) = &assign_stmt.lhs[0] {
                        let var_name = objs.idents[*var_key].name.clone();
                        let value_expr = self.translate_expression(&assign_stmt.rhs[0], objs);

                        match assign_stmt.token {
                            Token::ASSIGN | Token::DEFINE => {
                                Some(WasmStatement::VarDecl(var_name, value_expr))
                            }
                            _ => {
                                println!(
                                    "Warning: Unsupported assignment operator: {:?}",
                                    assign_stmt.token
                                );
                                Some(WasmStatement::VarDecl(var_name, value_expr))
                            }
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Stmt::Return(return_stmt) => {
                let return_expr = if !return_stmt.results.is_empty() {
                    Some(self.translate_expression(&return_stmt.results[0], objs))
                } else {
                    None
                };
                Some(WasmStatement::Return(return_expr))
            }
            Stmt::Block(block) => {
                let statements = self.translate_statements(&block.list, objs);
                Some(WasmStatement::Block(statements))
            }
            Stmt::If(if_stmt) => {
                let condition = self.translate_expression(&if_stmt.cond, objs);
                let if_statements = self.translate_statements(&if_stmt.body.list, objs);
                let else_statements = if let Some(ref else_stmt) = if_stmt.els {
                    match else_stmt {
                        Stmt::Block(else_block) => {
                            Some(self.translate_statements(&else_block.list, objs))
                        }
                        _ => Some(vec![self.translate_statement_optional(else_stmt, objs)?]),
                    }
                } else {
                    None
                };
                Some(WasmStatement::If(condition, if_statements, else_statements))
            }
            Stmt::IncDec(inc_dec) => {
                if let Expr::Ident(var_key) = &inc_dec.expr {
                    let var_name = objs.idents[*var_key].name.clone();
                    let operation = match inc_dec.token {
                        Token::INC => WasmBinaryOp::Add,
                        Token::DEC => WasmBinaryOp::Sub,
                        _ => {
                            println!(
                                "Warning: Unknown increment/decrement operator: {:?}",
                                inc_dec.token
                            );
                            WasmBinaryOp::Add
                        }
                    };

                    let increment_expr = WasmExpr::Binary(
                        operation,
                        Box::new(WasmExpr::Variable(var_name.clone())),
                        Box::new(WasmExpr::Integer(1)),
                    );

                    Some(WasmStatement::VarDecl(var_name, increment_expr))
                } else {
                    println!("Warning: Skipping complex increment/decrement");
                    None
                }
            }
            Stmt::For(for_stmt) => {
                let mut loop_body = Vec::new();

                // Add condition check at start of loop (if present)
                if let Some(ref condition) = for_stmt.cond {
                    // If condition is false, break out of loop
                    let negated_condition = WasmExpr::Unary(
                        WasmUnaryOp::Not,
                        Box::new(self.translate_expression(condition, objs)),
                    );
                    loop_body.push(WasmStatement::If(
                        negated_condition,
                        vec![WasmStatement::Break],
                        None,
                    ));
                }

                // Add the main body statements
                let body_statements = self.translate_statements(&for_stmt.body.list, objs);
                loop_body.extend(body_statements);

                // Add post statement (if present)
                if let Some(ref post_stmt) = for_stmt.post
                    && let Some(post_wasm) = self.translate_statement_optional(post_stmt, objs)
                {
                    loop_body.push(post_wasm);
                }

                // Wrap everything in a block that includes init + loop
                let mut statements = Vec::new();

                // Add init statement (if present)
                if let Some(ref init_stmt) = for_stmt.init
                    && let Some(init_wasm) = self.translate_statement_optional(init_stmt, objs)
                {
                    statements.push(init_wasm);
                }

                // Add the loop
                statements.push(WasmStatement::Loop(loop_body));

                Some(WasmStatement::Block(statements))
            }
            Stmt::Switch(switch_stmt) => {
                // Get the tag expression
                let tag_expr = if let Some(ref tag) = switch_stmt.tag {
                    self.translate_expression(tag, objs)
                } else {
                    // If no tag, it's like switch true
                    WasmExpr::Integer(1)
                };

                // Translate each case clause
                let mut cases = Vec::new();
                for stmt in &switch_stmt.body.list {
                    if let Stmt::Case(case_clause) = stmt {
                        // Extract case values (None means default case)
                        let case_values = if let Some(ref list) = case_clause.list {
                            if list.is_empty() {
                                None
                            } else {
                                Some(
                                    list.iter()
                                        .map(|expr| self.translate_expression(expr, objs))
                                        .collect(),
                                )
                            }
                        } else {
                            None
                        };

                        // Translate case body
                        let case_body = self.translate_statements(&case_clause.body, objs);
                        cases.push((case_values, case_body));
                    }
                }

                Some(WasmStatement::Switch(tag_expr, cases))
            }
            Stmt::Decl(decl) => {
                match decl.as_ref() {
                    Decl::Gen(gen_decl) => {
                        // Handle type declarations (struct, etc.)
                        for spec_key in &gen_decl.specs {
                            let spec = &objs.specs[*spec_key];
                            if let Spec::Type(type_spec) = spec {
                                // Get the type name
                                let type_name = objs.idents[type_spec.name].name.clone();

                                // Check if it's a struct type
                                if let Expr::Struct(struct_type) = &type_spec.typ {
                                    // Extract struct fields
                                    let mut fields = Vec::new();
                                    for field_key in &struct_type.fields.list {
                                        let field = &objs.fields[*field_key];
                                        for name_key in &field.names {
                                            let field_name = objs.idents[*name_key].name.clone();
                                            // For now, assume all fields are int
                                            fields.push((field_name, WasmType::Int));
                                        }
                                    }

                                    return Some(WasmStatement::StructDecl(type_name, fields));
                                }
                            }
                        }
                        None
                    }
                    _ => None,
                }
            }
            _ => {
                println!(
                    "Warning: Skipping unsupported statement type: {:?}",
                    go_stmt
                );
                None
            }
        }
    }
}

// WASM Compiler (keeping the same implementation)
#[derive(Default)]
pub struct WasmCompiler {
    types: TypeSection,
    functions: FunctionSection,
    imports: ImportSection,
    exports: ExportSection,
    codes: CodeSection,
    function_types: HashMap<String, u32>,
    function_indices: HashMap<String, u32>,
    variables: HashMap<String, u32>,
    variable_types: HashMap<String, String>, // Variable name -> struct type name
    string_values: HashMap<String, String>,  // Variable name -> string value
    next_local_index: u32,
    current_func_index: u32,
    struct_definitions: HashMap<String, WasmStructDef>,
    string_table: HashMap<String, u32>, // String content -> memory offset
    heap_pointer: u32,                  // Current heap allocation pointer
    imported_packages: Vec<String>,     // List of imported package names
}

impl WasmCompiler {
    pub fn new() -> Self {
        Self {
            types: TypeSection::new(),
            functions: FunctionSection::new(),
            imports: ImportSection::new(),
            exports: ExportSection::new(),
            codes: CodeSection::new(),
            function_types: HashMap::new(),
            function_indices: HashMap::new(),
            variables: HashMap::new(),
            variable_types: HashMap::new(),
            string_values: HashMap::new(),
            next_local_index: 0,
            current_func_index: 0,
            struct_definitions: HashMap::new(),
            string_table: HashMap::new(),
            heap_pointer: 1024, // Start heap after some reserved space
            imported_packages: Vec::new(),
        }
    }

    pub fn compile_program(&mut self, program: &WasmProgram) -> Vec<u8> {
        // Store imported packages and struct definitions
        self.imported_packages = program.imported_packages.clone();
        for struct_def in &program.structs {
            self.struct_definitions
                .insert(struct_def.name.clone(), struct_def.clone());
        }

        // First pass: register all import signatures
        for import in &program.imports {
            let type_index = self.types.len();
            self.types
                .ty()
                .function(import.params.clone(), import.results.clone());
            self.function_types
                .insert(format!("{}.{}", import.module, import.name), type_index);
            self.imports.import(
                &import.module,
                &import.name,
                EntityType::Function(type_index),
            );
            self.function_indices.insert(
                format!("{}.{}", import.module, import.name),
                self.current_func_index,
            );
            self.current_func_index += 1;
        }

        // Second pass: register all function signatures
        for func in &program.functions {
            let param_types: Vec<ValType> = func
                .params
                .iter()
                .map(|(_, ty)| match ty {
                    WasmType::Int | WasmType::Uint | WasmType::Uint32 => ValType::I32,
                    WasmType::Int8 | WasmType::Uint8 | WasmType::Bool => ValType::I32,
                    WasmType::Int16 | WasmType::Uint16 => ValType::I32,
                    WasmType::Int64 | WasmType::Uint64 => ValType::I64,
                    WasmType::Float => ValType::F32,
                    WasmType::Float64 => ValType::F64,
                    WasmType::Void => panic!("Void cannot be a parameter type"),
                    WasmType::String => ValType::I32, // String as pointer
                    WasmType::Struct(_) => ValType::I32, // Treat structs as pointers
                    WasmType::Pointer(_) => ValType::I32,
                    WasmType::Array(_, _) => ValType::I32, // Arrays as pointers
                    WasmType::Slice(_) => ValType::I32,    // Slices as pointers
                })
                .collect();

            let return_types: Vec<ValType> = match &func.return_type {
                Some(WasmType::Int) | Some(WasmType::Uint) | Some(WasmType::Uint32) => {
                    vec![ValType::I32]
                }
                Some(WasmType::Int8) | Some(WasmType::Uint8) | Some(WasmType::Bool) => {
                    vec![ValType::I32]
                }
                Some(WasmType::Int16) | Some(WasmType::Uint16) => vec![ValType::I32],
                Some(WasmType::Int64) | Some(WasmType::Uint64) => vec![ValType::I64],
                Some(WasmType::Float) => vec![ValType::F32],
                Some(WasmType::Float64) => vec![ValType::F64],
                Some(WasmType::String) => vec![ValType::I32], // Return as pointer
                Some(WasmType::Struct(_)) => vec![ValType::I32], // Return as pointer
                Some(WasmType::Pointer(_)) => vec![ValType::I32],
                Some(WasmType::Array(_, _)) => vec![ValType::I32], // Return as pointer
                Some(WasmType::Slice(_)) => vec![ValType::I32],    // Return as pointer
                Some(WasmType::Void) | None => vec![],
            };

            let type_index = self.types.len();
            self.types.ty().function(param_types, return_types);
            self.function_types.insert(func.name.clone(), type_index);
            self.function_indices
                .insert(func.name.clone(), self.current_func_index);
            self.current_func_index += 1;
        }

        // Reset function index for compilation
        self.current_func_index = 0;

        // Second pass: compile function bodies
        for func in &program.functions {
            self.compile_function(func);
        }

        let mut module = WasmModule::new();

        // Add sections in the correct order according to WebAssembly specification:
        // 1. Type section
        module.section(&self.types);

        // 2. Import section
        module.section(&self.imports);

        // 3. Function section
        module.section(&self.functions);

        // 4. Memory section (for struct storage)
        let mut memory_section = MemorySection::new();
        memory_section.memory(MemoryType {
            minimum: 1, // At least 1 page (64KB)
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        module.section(&memory_section);

        // 5. Export section
        module.section(&self.exports);

        // 5. Code section (must come last)
        module.section(&self.codes);

        module.finish()
    }

    fn try_import_function(&mut self, func_name: &str) -> Option<u32> {
        // Check if this is a qualified function name like "pkg.Function"
        if let Some(dot_pos) = func_name.find('.') {
            let pkg_name = &func_name[..dot_pos];
            let func_part = &func_name[dot_pos + 1..];

            // Check if the package is imported
            if self.imported_packages.contains(&pkg_name.to_string()) {
                // Check if this is a supported stdlib function
                use crate::wasm::std;
                if let Some(import_info) = std::get_stdlib_import(pkg_name, func_part) {
                    // Create the import dynamically
                    let type_index = self.types.len();
                    self.types
                        .ty()
                        .function(import_info.params.clone(), import_info.results.clone());

                    // Add to imports section
                    self.imports.import(
                        &import_info.module,
                        &import_info.name,
                        EntityType::Function(type_index),
                    );

                    // Register the function
                    let func_idx = self.current_func_index;
                    self.function_indices
                        .insert(func_name.to_string(), func_idx);
                    self.current_func_index += 1;

                    return Some(func_idx);
                }
            }
        }
        None
    }

    fn compile_function(&mut self, func: &WasmFunctionDef) {
        // Reset state for this function
        self.variables.clear();
        self.variable_types.clear();
        self.string_values.clear();
        self.next_local_index = 0;

        // Register parameters as local variables
        let mut locals = Vec::new();
        for (i, (name, _)) in func.params.iter().enumerate() {
            self.variables.insert(name.clone(), i as u32);
            self.next_local_index += 1;
        }

        // Get function type index
        let type_index = *self.function_types.get(&func.name).unwrap();

        // Add function to function section
        self.functions.function(type_index);

        // Export if needed
        if let Some(export_name) = &func.export_name {
            self.exports
                .export(export_name, ExportKind::Func, self.current_func_index);
        }

        // Collect local variable declarations
        self.collect_variable_declarations(&func.body, &mut locals);

        // Create function with locals
        let mut f = Function::new(locals);

        // Reset next_local_index to account for parameters only
        self.next_local_index = func.params.len() as u32;

        // Compile function body
        for stmt in &func.body {
            self.compile_statement_with_indexing(stmt, &mut f);
        }

        // Ensure function ends properly
        // If function has a return type but doesn't explicitly return,
        // add unreachable to indicate this shouldn't be reached
        if func.return_type.is_some() {
            f.instruction(&Instruction::Unreachable);
        }

        f.instruction(&Instruction::End);
        self.codes.function(&f);

        self.current_func_index += 1;
    }

    #[allow(clippy::only_used_in_recursion)]
    fn collect_variable_declarations(
        &mut self,
        statements: &[WasmStatement],
        locals: &mut Vec<(u32, ValType)>,
    ) {
        for stmt in statements {
            match stmt {
                WasmStatement::VarDecl(_, _) => {
                    locals.push((1, ValType::I32));
                }
                WasmStatement::Block(block_stmts) => {
                    self.collect_variable_declarations(block_stmts, locals);
                }
                WasmStatement::If(_, if_stmts, else_stmts) => {
                    self.collect_variable_declarations(if_stmts, locals);
                    if let Some(else_statements) = else_stmts {
                        self.collect_variable_declarations(else_statements, locals);
                    }
                }
                WasmStatement::Loop(loop_stmts) => {
                    self.collect_variable_declarations(loop_stmts, locals);
                }
                WasmStatement::Switch(_, cases) => {
                    // Add a local for the tag value
                    locals.push((1, ValType::I32));
                    // Collect from case bodies
                    for (_, case_body) in cases {
                        self.collect_variable_declarations(case_body, locals);
                    }
                }
                WasmStatement::StructDecl(_, _) => {
                    // Struct declarations don't need local variables
                }
                _ => {}
            }
        }
    }

    fn compile_statement_with_indexing(&mut self, stmt: &WasmStatement, f: &mut Function) {
        match stmt {
            WasmStatement::ExprStmt(expr) => {
                self.compile_expression(expr, f, &mut Vec::new());
                f.instruction(&Instruction::Drop); // Drop expression result
            }
            WasmStatement::VarDecl(name, init_expr) => {
                // Track the type if this is a struct literal
                if let WasmExpr::StructLiteral(struct_name, _) = init_expr {
                    self.variable_types
                        .insert(name.clone(), struct_name.clone());
                }

                // Track string values for len() support
                if let WasmExpr::String(content) = init_expr {
                    self.string_values.insert(name.clone(), content.clone());
                }

                self.compile_expression(init_expr, f, &mut Vec::new());

                let local_idx = if let Some(&existing_idx) = self.variables.get(name) {
                    existing_idx
                } else {
                    let idx = self.next_local_index;
                    self.variables.insert(name.clone(), idx);
                    self.next_local_index += 1;
                    idx
                };

                f.instruction(&Instruction::LocalSet(local_idx));
            }
            WasmStatement::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.compile_expression(expr, f, &mut Vec::new());
                }
                f.instruction(&Instruction::Return);
            }
            WasmStatement::Block(stmts) => {
                // Just compile each statement in sequence
                for s in stmts {
                    self.compile_statement_with_indexing(s, f);
                }
            }
            WasmStatement::If(condition, if_stmts, else_stmts) => {
                self.compile_expression(condition, f, &mut Vec::new());
                f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

                for stmt in if_stmts {
                    self.compile_statement_with_indexing(stmt, f);
                }

                if let Some(else_statements) = else_stmts {
                    f.instruction(&Instruction::Else);
                    for stmt in else_statements {
                        self.compile_statement_with_indexing(stmt, f);
                    }
                }

                f.instruction(&Instruction::End);
            }
            WasmStatement::Loop(body_stmts) => {
                // Simple loop: block { loop { body; br 0 } }
                f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
                f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));

                for stmt in body_stmts {
                    if let WasmStatement::If(condition, if_stmts, _) = stmt
                        && if_stmts.iter().any(|s| matches!(s, WasmStatement::Break))
                    {
                        // This is a break condition
                        self.compile_expression(condition, f, &mut Vec::new());
                        f.instruction(&Instruction::BrIf(1)); // Break out of loop
                        continue;
                    }

                    self.compile_statement_with_indexing(stmt, f);
                }

                f.instruction(&Instruction::Br(0)); // Continue loop
                f.instruction(&Instruction::End); // End loop
                f.instruction(&Instruction::End); // End block
            }
            WasmStatement::Break => {
                f.instruction(&Instruction::Br(1)); // Break out of loop
            }
            WasmStatement::Switch(tag_expr, cases) => {
                // Compile the tag expression once and store in a local
                self.compile_expression(tag_expr, f, &mut Vec::new());
                let tag_local = self.next_local_index;
                self.next_local_index += 1;
                f.instruction(&Instruction::LocalSet(tag_local));

                // Build if-else chain: check each case sequentially
                let non_default_cases: Vec<_> =
                    cases.iter().filter(|(vals, _)| vals.is_some()).collect();

                for (i, (case_values, case_body)) in non_default_cases.iter().enumerate() {
                    if let Some(values) = case_values {
                        // Build condition: tag == value1 || tag == value2 || ...
                        for (j, value) in values.iter().enumerate() {
                            f.instruction(&Instruction::LocalGet(tag_local));
                            self.compile_expression(value, f, &mut Vec::new());
                            f.instruction(&Instruction::I32Eq);

                            if j > 0 {
                                f.instruction(&Instruction::I32Or);
                            }
                        }

                        // Start if block
                        f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

                        // Execute case body
                        for stmt in case_body {
                            self.compile_statement_with_indexing(stmt, f);
                        }

                        // If this isn't the last case, we need an else
                        if i < non_default_cases.len() - 1 || cases.iter().any(|(v, _)| v.is_none())
                        {
                            f.instruction(&Instruction::Else);
                        }
                    }
                }

                // Handle default case if it exists
                for (case_values, case_body) in cases {
                    if case_values.is_none() {
                        for stmt in case_body {
                            self.compile_statement_with_indexing(stmt, f);
                        }
                        break;
                    }
                }

                // Close all the if blocks
                for _ in 0..non_default_cases.len() {
                    f.instruction(&Instruction::End);
                }
            }
            WasmStatement::StructDecl(struct_name, fields) => {
                // Register the struct definition for later use
                // Calculate field offsets
                let mut field_offsets = HashMap::new();
                let mut current_offset = 0u32;

                for (field_name, field_type) in fields {
                    field_offsets.insert(field_name.clone(), current_offset);
                    // Calculate size based on type (simplified)
                    let field_size = match field_type {
                        WasmType::Int | WasmType::Float | WasmType::Pointer(_) => 4,
                        WasmType::String | WasmType::Slice(_) => 8,
                        _ => 4, // Default
                    };
                    current_offset += field_size;
                }

                let struct_def = WasmStructDef {
                    name: struct_name.clone(),
                    fields: fields.clone(),
                    field_offsets,
                    size: current_offset,
                };

                self.struct_definitions
                    .insert(struct_name.clone(), struct_def);
                // Struct declarations are compile-time constructs, no runtime code needed
            }
        }
    }

    fn compile_expression(
        &mut self,
        expr: &WasmExpr,
        f: &mut Function,
        _locals: &mut Vec<(u32, ValType)>,
    ) {
        match expr {
            WasmExpr::Integer(value) => {
                f.instruction(&Instruction::I32Const(*value));
            }
            WasmExpr::String(content) => {
                // Allocate string in memory if not already stored
                let offset = if let Some(&existing_offset) = self.string_table.get(content) {
                    existing_offset
                } else {
                    let offset = self.heap_pointer;
                    let bytes = content.as_bytes();

                    // Store length at offset
                    // Store string data starting at offset + 4
                    // For now, return pointer to string data
                    // In a full implementation, we'd write to a data section

                    self.string_table.insert(content.clone(), offset);
                    self.heap_pointer += 4 + bytes.len() as u32;
                    offset
                };

                // Push pointer to string (offset + 4 for data, after length)
                f.instruction(&Instruction::I32Const((offset + 4) as i32));
            }
            WasmExpr::Variable(name) => {
                if let Some(&idx) = self.variables.get(name) {
                    f.instruction(&Instruction::LocalGet(idx));
                } else {
                    // Variable doesn't exist - just push 0
                    f.instruction(&Instruction::I32Const(0));
                }
            }
            WasmExpr::Binary(op, left, right) => {
                // Compile left and right operands
                self.compile_expression(left, f, _locals);
                self.compile_expression(right, f, _locals);

                // Apply the operation
                match op {
                    WasmBinaryOp::Add => {
                        f.instruction(&Instruction::I32Add);
                    }
                    WasmBinaryOp::Sub => {
                        f.instruction(&Instruction::I32Sub);
                    }
                    WasmBinaryOp::Mul => {
                        f.instruction(&Instruction::I32Mul);
                    }
                    WasmBinaryOp::Div => {
                        f.instruction(&Instruction::I32DivS);
                    }
                    WasmBinaryOp::Rem => {
                        f.instruction(&Instruction::I32RemS);
                    }
                    WasmBinaryOp::Lt => {
                        f.instruction(&Instruction::I32LtS);
                    }
                    WasmBinaryOp::Gt => {
                        f.instruction(&Instruction::I32GtS);
                    }
                    WasmBinaryOp::LtEq => {
                        f.instruction(&Instruction::I32LeS);
                    }
                    WasmBinaryOp::GtEq => {
                        f.instruction(&Instruction::I32GeS);
                    }
                    WasmBinaryOp::Eq => {
                        f.instruction(&Instruction::I32Eq);
                    }
                    WasmBinaryOp::NotEq => {
                        f.instruction(&Instruction::I32Ne);
                    }
                    WasmBinaryOp::And => {
                        f.instruction(&Instruction::I32And);
                    }
                    WasmBinaryOp::Or => {
                        f.instruction(&Instruction::I32Or);
                    }
                    WasmBinaryOp::Xor => {
                        f.instruction(&Instruction::I32Xor);
                    }
                    WasmBinaryOp::Shl => {
                        f.instruction(&Instruction::I32Shl);
                    }
                    WasmBinaryOp::Shr => {
                        f.instruction(&Instruction::I32ShrS);
                    }
                    WasmBinaryOp::LogicalOr => {
                        // Simple: a || b becomes (a | b) != 0
                        f.instruction(&Instruction::I32Or);
                        f.instruction(&Instruction::I32Const(0));
                        f.instruction(&Instruction::I32Ne);
                    }
                    WasmBinaryOp::LogicalAnd => {
                        // Simple: a && b becomes (a & b) != 0
                        f.instruction(&Instruction::I32And);
                        f.instruction(&Instruction::I32Const(0));
                        f.instruction(&Instruction::I32Ne);
                    }
                }
            }
            WasmExpr::Call(func_name, args) => {
                // Compile arguments
                for arg in args {
                    self.compile_expression(arg, f, _locals);
                }

                // Call function
                if let Some(&func_idx) = self.function_indices.get(func_name) {
                    f.instruction(&Instruction::Call(func_idx));
                } else {
                    // Check if this is an imported function
                    if let Some(func_idx) = self.try_import_function(func_name) {
                        f.instruction(&Instruction::Call(func_idx));
                    } else {
                        // Unknown function - drop args and push 0
                        for _ in 0..args.len() {
                            f.instruction(&Instruction::Drop);
                        }
                        f.instruction(&Instruction::I32Const(0));
                    }
                }
            }
            WasmExpr::Assign(name, value) => {
                // Compile value and store in variable
                self.compile_expression(value, f, _locals);
                if let Some(&idx) = self.variables.get(name) {
                    f.instruction(&Instruction::LocalTee(idx)); // Store and keep value on stack
                } else {
                    // Create new variable
                    let idx = self.next_local_index;
                    self.variables.insert(name.clone(), idx);
                    self.next_local_index += 1;
                    f.instruction(&Instruction::LocalTee(idx));
                }
            }
            WasmExpr::Unary(op, operand) => {
                match op {
                    WasmUnaryOp::Neg => {
                        // Negation: 0 - operand
                        f.instruction(&Instruction::I32Const(0));
                        self.compile_expression(operand, f, _locals);
                        f.instruction(&Instruction::I32Sub);
                    }
                    WasmUnaryOp::Not => {
                        // Logical not: operand == 0
                        self.compile_expression(operand, f, _locals);
                        f.instruction(&Instruction::I32Eqz);
                    }
                }
            }
            WasmExpr::StructLiteral(struct_name, field_values) => {
                // Allocate memory for struct
                if let Some(struct_def) = self.struct_definitions.get(struct_name).cloned() {
                    let struct_ptr = self.heap_pointer;

                    // Initialize each field
                    for (field_name, field_expr) in field_values {
                        if let Some(&offset) = struct_def.field_offsets.get(field_name) {
                            // Push field address: struct_base + offset
                            f.instruction(&Instruction::I32Const(struct_ptr as i32));

                            // Compile and push field value
                            self.compile_expression(field_expr, f, _locals);

                            // Store field value at calculated address (using offset in MemArg)
                            f.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                                offset: offset as u64,
                                align: 2, // 4-byte alignment
                                memory_index: 0,
                            }));
                        }
                    }

                    // Push struct pointer as the result
                    f.instruction(&Instruction::I32Const(struct_ptr as i32));

                    // Update heap pointer for next allocation
                    self.heap_pointer += struct_def.size;
                } else {
                    // Unknown struct type
                    f.instruction(&Instruction::I32Const(0));
                }
            }
            WasmExpr::FieldAccess(object_expr, field_name) => {
                // We need to track the struct type through the expression
                // For now, we'll infer it from the object expression
                let field_offset = self.infer_field_offset(object_expr, field_name);
                let field_type = self.infer_field_type(object_expr, field_name);

                // Compile object expression to get struct pointer
                self.compile_expression(object_expr, f, _locals);

                // Load value from field address (using offset in MemArg)
                // Use appropriate load instruction based on field type
                match field_type {
                    WasmType::Int8 => {
                        // Load signed 8-bit, extend to i32
                        f.instruction(&Instruction::I32Load8S(wasm_encoder::MemArg {
                            offset: field_offset as u64,
                            align: 0,
                            memory_index: 0,
                        }));
                    }
                    WasmType::Uint8 | WasmType::Bool => {
                        // Load unsigned 8-bit, extend to i32
                        f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
                            offset: field_offset as u64,
                            align: 0,
                            memory_index: 0,
                        }));
                    }
                    WasmType::Int16 => {
                        // Load signed 16-bit, extend to i32
                        f.instruction(&Instruction::I32Load16S(wasm_encoder::MemArg {
                            offset: field_offset as u64,
                            align: 1,
                            memory_index: 0,
                        }));
                    }
                    WasmType::Uint16 => {
                        // Load unsigned 16-bit, extend to i32
                        f.instruction(&Instruction::I32Load16U(wasm_encoder::MemArg {
                            offset: field_offset as u64,
                            align: 1,
                            memory_index: 0,
                        }));
                    }
                    WasmType::Int64 | WasmType::Uint64 => {
                        // Load 64-bit integer
                        f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                            offset: field_offset as u64,
                            align: 3,
                            memory_index: 0,
                        }));
                        // Convert to i32 for now (wrap)
                        f.instruction(&Instruction::I32WrapI64);
                    }
                    WasmType::Float => {
                        // Load 32-bit float
                        f.instruction(&Instruction::F32Load(wasm_encoder::MemArg {
                            offset: field_offset as u64,
                            align: 2,
                            memory_index: 0,
                        }));
                    }
                    WasmType::Float64 => {
                        // Load 64-bit float
                        f.instruction(&Instruction::F64Load(wasm_encoder::MemArg {
                            offset: field_offset as u64,
                            align: 3,
                            memory_index: 0,
                        }));
                    }
                    _ => {
                        // Default: load 32-bit integer
                        f.instruction(&Instruction::I32Load(wasm_encoder::MemArg {
                            offset: field_offset as u64,
                            align: 2,
                            memory_index: 0,
                        }));
                    }
                }
            }
            WasmExpr::FieldAssign(object_expr, field_name, value_expr) => {
                // Use the special helper for field assignments
                self.compile_field_assign(object_expr, field_name, value_expr, f, _locals);
            }
            WasmExpr::AddressOf(expr) => {
                // For variables, return their memory address
                // This is simplified - proper implementation would need address tracking
                match expr.as_ref() {
                    WasmExpr::Variable(_var_name) => {
                        // Return a mock address for the variable
                        f.instruction(&Instruction::I32Const(self.heap_pointer as i32));
                        self.heap_pointer += 4; // Reserve space
                    }
                    _ => {
                        self.compile_expression(expr, f, _locals);
                        // For now, just return the value itself
                    }
                }
            }
            WasmExpr::Dereference(expr) => {
                // Load value from address
                self.compile_expression(expr, f, _locals);
                f.instruction(&Instruction::I32Load(wasm_encoder::MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
            }
            WasmExpr::ArrayLiteral(elements) => {
                // Allocate memory for array
                let element_size = 4; // 4 bytes per i32 element
                let array_size = elements.len() as u32 * element_size;
                let array_ptr = self.heap_pointer;

                // Initialize each element
                for (i, element_expr) in elements.iter().enumerate() {
                    let offset = i as u32 * element_size;

                    // Push array base address
                    f.instruction(&Instruction::I32Const(array_ptr as i32));

                    // Compile element value
                    self.compile_expression(element_expr, f, _locals);

                    // Store element at calculated offset
                    f.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                        offset: offset as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                }

                // Push array pointer as result
                f.instruction(&Instruction::I32Const(array_ptr as i32));

                // Update heap pointer
                self.heap_pointer += array_size;
            }
            WasmExpr::IndexAccess(array_expr, index_expr) => {
                // Compile array expression to get base pointer
                self.compile_expression(array_expr, f, _locals);

                // Compile index expression
                self.compile_expression(index_expr, f, _locals);

                // Multiply index by element size (4 bytes for i32)
                f.instruction(&Instruction::I32Const(4));
                f.instruction(&Instruction::I32Mul);

                // Add to base pointer to get element address
                f.instruction(&Instruction::I32Add);

                // Load value from calculated address
                f.instruction(&Instruction::I32Load(wasm_encoder::MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
            }
            WasmExpr::IndexAssign(array_expr, index_expr, value_expr) => {
                // Compile array expression to get base pointer
                self.compile_expression(array_expr, f, _locals);

                // Compile index expression
                self.compile_expression(index_expr, f, _locals);

                // Multiply index by element size (4 bytes for i32)
                f.instruction(&Instruction::I32Const(4));
                f.instruction(&Instruction::I32Mul);

                // Add to base pointer to get element address
                f.instruction(&Instruction::I32Add);

                // Compile value to store
                self.compile_expression(value_expr, f, _locals);

                // Store value at calculated address
                f.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));

                // Push dummy value for expression semantics
                f.instruction(&Instruction::I32Const(0));
            }
            WasmExpr::MakeSlice(size) => {
                // Allocate memory for slice data
                let element_size = 4; // 4 bytes per i32 element
                let data_size = (*size as u32) * element_size;
                let data_ptr = self.heap_pointer;

                // Zero-initialize the array
                for i in 0..*size {
                    let offset = (i as u32) * element_size;
                    f.instruction(&Instruction::I32Const(data_ptr as i32));
                    f.instruction(&Instruction::I32Const(0));
                    f.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                        offset: offset as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                }

                // Update heap pointer
                self.heap_pointer += data_size;

                // Return pointer to the allocated array
                f.instruction(&Instruction::I32Const(data_ptr as i32));
            }
            WasmExpr::StringLen(string_expr) => {
                // Get the string length
                match string_expr.as_ref() {
                    WasmExpr::String(content) => {
                        // Direct string literal
                        f.instruction(&Instruction::I32Const(content.len() as i32));
                    }
                    WasmExpr::Variable(var_name) => {
                        // Look up the string value for this variable
                        if let Some(string_content) = self.string_values.get(var_name) {
                            f.instruction(&Instruction::I32Const(string_content.len() as i32));
                        } else {
                            // Variable not found or not a string, return 0
                            f.instruction(&Instruction::I32Const(0));
                        }
                    }
                    _ => {
                        // For other expressions, return 0 as fallback
                        f.instruction(&Instruction::I32Const(0));
                    }
                }
            }
            WasmExpr::StringConcat(left_expr, right_expr) => {
                // Simplified string concatenation
                // In a full implementation, this would allocate new memory and copy both strings
                // For now, we just return the first string's pointer
                self.compile_expression(left_expr, f, _locals);
                // Drop the second string for now
                self.compile_expression(right_expr, f, _locals);
                f.instruction(&Instruction::Drop);
            }
        }
    }

    // Special helper for field assignments that need proper stack management
    fn compile_field_assign(
        &mut self,
        object_expr: &WasmExpr,
        field_name: &str,
        value_expr: &WasmExpr,
        f: &mut Function,
        _locals: &mut Vec<(u32, ValType)>,
    ) {
        let field_offset = self.infer_field_offset(object_expr, field_name);
        let field_type = self.infer_field_type(object_expr, field_name);

        // Compile object expression to get struct pointer
        self.compile_expression(object_expr, f, _locals);

        // Compile value to store
        self.compile_expression(value_expr, f, _locals);

        // Now stack is: [ptr, value]
        // Store consumes both, leaving stack empty
        // Use appropriate store instruction based on field type
        match field_type {
            WasmType::Int8 | WasmType::Uint8 | WasmType::Bool => {
                // Store 8-bit value
                f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
                    offset: field_offset as u64,
                    align: 0,
                    memory_index: 0,
                }));
            }
            WasmType::Int16 | WasmType::Uint16 => {
                // Store 16-bit value
                f.instruction(&Instruction::I32Store16(wasm_encoder::MemArg {
                    offset: field_offset as u64,
                    align: 1,
                    memory_index: 0,
                }));
            }
            WasmType::Int64 | WasmType::Uint64 => {
                // Convert i32 to i64 and store
                f.instruction(&Instruction::I64ExtendI32S);
                f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                    offset: field_offset as u64,
                    align: 3,
                    memory_index: 0,
                }));
            }
            WasmType::Float => {
                // Store 32-bit float
                f.instruction(&Instruction::F32Store(wasm_encoder::MemArg {
                    offset: field_offset as u64,
                    align: 2,
                    memory_index: 0,
                }));
            }
            WasmType::Float64 => {
                // Store 64-bit float
                f.instruction(&Instruction::F64Store(wasm_encoder::MemArg {
                    offset: field_offset as u64,
                    align: 3,
                    memory_index: 0,
                }));
            }
            _ => {
                // Default: store 32-bit integer
                f.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                    offset: field_offset as u64,
                    align: 2,
                    memory_index: 0,
                }));
            }
        }

        // For expression semantics, we should return the value
        // But store instructions don't leave anything on stack
        // So we push a dummy value (0) to satisfy the Drop
        f.instruction(&Instruction::I32Const(0));
    }

    // Helper method to infer the field offset for field access
    fn infer_field_offset(&self, object_expr: &WasmExpr, field_name: &str) -> u32 {
        // Try to determine the struct type from the object expression
        match object_expr {
            WasmExpr::Variable(var_name) => {
                // Look up the variable's type
                if let Some(struct_type_name) = self.variable_types.get(var_name)
                    && let Some(struct_def) = self.struct_definitions.get(struct_type_name)
                    && let Some(&offset) = struct_def.field_offsets.get(field_name)
                {
                    return offset;
                }
            }
            WasmExpr::StructLiteral(struct_name, _) => {
                // Direct struct literal access
                if let Some(struct_def) = self.struct_definitions.get(struct_name)
                    && let Some(&offset) = struct_def.field_offsets.get(field_name)
                {
                    return offset;
                }
            }
            _ => {}
        }

        // Fallback: return 0 if we can't determine the type
        0
    }

    // Helper method to infer the field type for field access
    fn infer_field_type(&self, object_expr: &WasmExpr, field_name: &str) -> WasmType {
        // Try to determine the struct type from the object expression
        match object_expr {
            WasmExpr::Variable(var_name) => {
                // Look up the variable's type
                if let Some(struct_type_name) = self.variable_types.get(var_name)
                    && let Some(struct_def) = self.struct_definitions.get(struct_type_name)
                {
                    for (fname, ftype) in &struct_def.fields {
                        if fname == field_name {
                            return ftype.clone();
                        }
                    }
                }
            }
            WasmExpr::StructLiteral(struct_name, _) => {
                // Direct struct literal access
                if let Some(struct_def) = self.struct_definitions.get(struct_name) {
                    for (fname, ftype) in &struct_def.fields {
                        if fname == field_name {
                            return ftype.clone();
                        }
                    }
                }
            }
            _ => {}
        }

        // Fallback: return Int if we can't determine the type
        WasmType::Int
    }
}

// Update the high-level compile function
pub fn compile_str(go_source: &str) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    // Parse the Go source code using the simple parser function
    let (ast_objects, go_program) =
        parse_str(go_source).map_err(|e| format!("Parse error: {}", e))?;

    // Translate Go AST to WASM AST
    let wasm_program = GoToWasmTranslator::translate_program(&go_program, &ast_objects, go_source);

    // Compile WASM AST to WASM bytecode
    let mut compiler = WasmCompiler::new();
    let wasm_bytes = compiler.compile_program(&wasm_program);

    Ok(wasm_bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_function_compilation() {
        let go_source = r#"
            package main

            func add(x int, y int) int {
                return x + y
            }
        "#;

        let result = compile_str(go_source);
        assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

        let wasm_bytes = result.unwrap();
        assert!(!wasm_bytes.is_empty(), "Generated WASM should not be empty");
    }

    #[test]
    fn test_import_compilation() {
        let go_source = r#"
            package main

            import "fmt"

            func main() {
                fmt.Println("Hello, World!")
            }
        "#;

        let result = compile_str(go_source);
        assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

        let wasm_bytes = result.unwrap();
        assert!(!wasm_bytes.is_empty(), "Generated WASM should not be empty");

        // Check that the WASM contains an import for the print function
        // We can't easily check the import section without wasm parsing,
        // but successful compilation indicates the import was processed
    }

    #[test]
    fn test_strings_join_import() {
        let go_source = r#"
            package main

            import "strings"

            //export test_join
            func test_join() {
                // Simple call to strings.Join - this verifies the import is created
                // The actual string operations are complex and would require
                // full Go string/slice runtime support
                strings.Join(nil, "")
            }
        "#;

        let result = compile_str(go_source);
        assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

        let wasm_bytes = result.unwrap();
        assert!(!wasm_bytes.is_empty(), "Generated WASM should not be empty");

        // Verify the WASM contains the strings_join import
        use wasmparser::{Parser, Payload};

        let parser = Parser::new(0);
        let mut has_strings_join_import = false;

        for payload in parser.parse_all(&wasm_bytes) {
            if let Ok(Payload::ImportSection(import_section)) = payload {
                for import in import_section {
                    if let Ok(import) = import
                        && import.module == "env"
                        && import.name == "strings_join"
                    {
                        has_strings_join_import = true;
                        break;
                    }
                }
            }
        }

        assert!(
            has_strings_join_import,
            "WASM should contain strings_join import"
        );
    }

    #[test]
    fn test_strings_join_execution() {
        // Test that the strings_join import can be called successfully
        // This is a simplified test that verifies the import mechanism works

        let go_source = r#"
            package main

            import "strings"

            func main() {
                // This test just verifies that strings.Join can be called
                // The actual implementation details are tested separately
                strings.Join(nil, "")
            }
        "#;

        let result = compile_str(go_source);
        assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

        let wasm_bytes = result.unwrap();

        // Verify the WASM contains the expected import
        use wasmparser::{Parser, Payload};

        let parser = Parser::new(0);
        let mut has_strings_join_import = false;

        for payload in parser.parse_all(&wasm_bytes) {
            if let Ok(Payload::ImportSection(import_section)) = payload {
                for import in import_section {
                    if let Ok(import) = import
                        && import.module == "env"
                        && import.name == "strings_join"
                    {
                        has_strings_join_import = true;
                    }
                }
            }
        }

        assert!(
            has_strings_join_import,
            "WASM should contain strings_join import"
        );

        // The test passes if compilation succeeds and the import is present
        // Full execution testing would require a complete WASM runtime with memory management
    }
}
