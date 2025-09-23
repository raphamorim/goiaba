// Copyright 2025 Raphael Amorim. All rights reserved.
// Use of this source code is governed by a GPL-3.0
// license that can be found in the LICENSE file.

use crate::parser::FuncTypeKey;
use crate::parser::Token;
use crate::parser::parse_str;
use std::collections::HashMap;
use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction,
    Module as WasmModule, TypeSection, ValType,
};

// Import your internal parser types
use crate::parser::{
    ast::{BasicLit, Decl, Expr, File, FuncDecl, Stmt},
    errors::ErrorList,
    objects::{AstObjects, IdentKey},
    parser::Parser,
    position,
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

// Helper function to convert IdentKey to String
fn ident_key_to_string(ident: &IdentKey) -> String {
    // This assumes IdentKey has some way to convert to string
    // You'll need to adjust based on your IdentKey implementation
    format!("{:?}", ident) // Placeholder - replace with actual conversion
}

// Helper function to get literal value as string
fn basic_lit_to_string(lit: &BasicLit) -> String {
    // This assumes BasicLit has some way to get the string value
    // You'll need to adjust based on your BasicLit implementation
    format!("{:?}", lit) // Placeholder - replace with actual conversion
}

// Define our WASM-specific AST structures
#[derive(Debug, Clone)]
pub enum WasmExpr {
    Integer(i32),
    Variable(String),
    Binary(WasmBinaryOp, Box<WasmExpr>, Box<WasmExpr>),
    Unary(WasmUnaryOp, Box<WasmExpr>),
    Call(String, Vec<WasmExpr>),
    Assign(String, Box<WasmExpr>),
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
    Lt,
    Gt,
    LtEq,
    GtEq,
    Eq,
    NotEq,
}

#[derive(Debug, Clone)]
pub enum WasmStatement {
    ExprStmt(WasmExpr),
    VarDecl(String, WasmExpr),
    Return(Option<WasmExpr>),
    Block(Vec<WasmStatement>),
    If(WasmExpr, Vec<WasmStatement>, Option<Vec<WasmStatement>>),
    Loop(Vec<WasmStatement>),
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
    Float,
    Void,
}

#[derive(Debug)]
pub struct WasmProgram {
    pub functions: Vec<WasmFunctionDef>,
}

// Translator from Go AST to WASM AST
pub struct GoToWasmTranslator;

// Update the translate_program signature to accept AstObjects
// Alternative approach: Filter out empty statements during translation

impl GoToWasmTranslator {
    pub fn translate_program(go_program: &File, objs: &AstObjects, source: &str) -> WasmProgram {
        let mut functions = Vec::new();

        for decl in &go_program.decls {
            if let Decl::Func(func_decl_key) = decl {
                // Resolve the FuncDeclKey to get the actual FuncDecl
                let func_decl = &objs.fdecls[*func_decl_key];
                functions.push(Self::translate_function(func_decl, objs, source));
            }
        }

        WasmProgram { functions }
    }

    // Updated translate_function with proper signature extraction
    fn translate_function(go_func: &FuncDecl, objs: &AstObjects, source: &str) -> WasmFunctionDef {
        // Extract function name - resolve IdentKey to get the actual name
        let func_name = objs.idents[go_func.name].name.clone();

        // Extract function signature from the type
        let (params, return_type) = Self::extract_function_signature(&go_func.typ, objs);

        // Extract function body using filtered translation
        let body = if let Some(ref body_block) = go_func.body {
            Self::translate_statements(&body_block.list, objs)
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
                    "int" | "int32" | "int64" => WasmType::Int,
                    "float32" | "float64" => WasmType::Float,
                    _ => WasmType::Int, // Default to int for unknown types
                }
            }
            // Handle other type expressions as needed
            _ => WasmType::Int, // Default fallback
        }
    }
    fn translate_expression(go_expr: &Expr, objs: &AstObjects) -> WasmExpr {
        match go_expr {
            Expr::BasicLit(lit) => {
                // Extract the actual literal value from the token
                match &lit.token {
                    Token::INT(lit_val) => {
                        // Assuming your INT token contains the literal value
                        // You'll need to adjust this based on your Token::INT implementation
                        let value_str: &String = lit_val.as_ref();
                        WasmExpr::Integer(value_str.parse::<i32>().unwrap_or(0))
                    }
                    Token::CHAR(lit_val) => {
                        // Handle character literals
                        let value_str: &String = lit_val.as_ref();
                        // Convert first char to its ASCII value
                        WasmExpr::Integer(value_str.chars().next().unwrap_or('0') as i32)
                    }
                    _ => {
                        println!("Warning: Unsupported literal type: {:?}", lit.token);
                        WasmExpr::Integer(0)
                    }
                }
            }
            Expr::Ident(ident_key) => {
                let ident_name = objs.idents[*ident_key].name.clone();
                WasmExpr::Variable(ident_name)
            }
            Expr::Binary(binary_expr) => {
                let left = Self::translate_expression(&binary_expr.expr_a, objs);
                let right = Self::translate_expression(&binary_expr.expr_b, objs);
                let op = Self::translate_binary_op(&binary_expr.op);
                WasmExpr::Binary(op, Box::new(left), Box::new(right))
            }
            Expr::Call(call_expr) => {
                let func_name = if let Expr::Ident(func_ident) = &call_expr.func {
                    objs.idents[*func_ident].name.clone()
                } else {
                    "unknown".to_string()
                };
                let args = call_expr
                    .args
                    .iter()
                    .map(|arg| Self::translate_expression(arg, objs))
                    .collect();
                WasmExpr::Call(func_name, args)
            }
            Expr::Unary(unary_expr) => {
                let operand = Self::translate_expression(&unary_expr.expr, objs);
                let op = Self::translate_unary_op(&unary_expr.op);
                WasmExpr::Unary(op, Box::new(operand))
            }
            _ => {
                println!("Warning: Unsupported expression type: {:?}", go_expr);
                WasmExpr::Integer(0)
            }
        }
    }

    // Helper to translate binary operators using pattern matching
    fn translate_binary_op(go_op: &Token) -> WasmBinaryOp {
        match go_op {
            Token::ADD => WasmBinaryOp::Add,
            Token::SUB => WasmBinaryOp::Sub,
            Token::MUL => WasmBinaryOp::Mul,
            Token::QUO => WasmBinaryOp::Div,
            Token::LSS => WasmBinaryOp::Lt,
            Token::GTR => WasmBinaryOp::Gt,
            Token::LEQ => WasmBinaryOp::LtEq,
            Token::GEQ => WasmBinaryOp::GtEq,
            Token::EQL => WasmBinaryOp::Eq,
            Token::NEQ => WasmBinaryOp::NotEq,
            // Add support for additional operators based on your Token enum
            _ => {
                println!("Warning: Unknown binary operator: {:?}", go_op);
                WasmBinaryOp::Add // Default fallback
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

    // Helper function to filter and translate statements
    fn translate_statements(go_stmts: &[Stmt], objs: &AstObjects) -> Vec<WasmStatement> {
        go_stmts
            .iter()
            .filter_map(|stmt| Self::translate_statement_optional(stmt, objs))
            .collect()
    }

    // Also fix the increment/decrement handling
    fn translate_statement_optional(go_stmt: &Stmt, objs: &AstObjects) -> Option<WasmStatement> {
        match go_stmt {
            Stmt::Empty(_) => None,
            Stmt::Expr(expr) => Some(WasmStatement::ExprStmt(Self::translate_expression(
                expr, objs,
            ))),
            Stmt::Assign(assign_key) => {
                let assign_stmt = &objs.a_stmts[*assign_key];

                if !assign_stmt.lhs.is_empty() && !assign_stmt.rhs.is_empty() {
                    if let Expr::Ident(var_key) = &assign_stmt.lhs[0] {
                        let var_name = objs.idents[*var_key].name.clone();
                        let value_expr = Self::translate_expression(&assign_stmt.rhs[0], objs);

                        // Check if it's a simple assignment or a compound assignment
                        match assign_stmt.token {
                            Token::ASSIGN => Some(WasmStatement::VarDecl(var_name, value_expr)),
                            Token::DEFINE => Some(WasmStatement::VarDecl(var_name, value_expr)),
                            _ => {
                                println!(
                                    "Warning: Unsupported assignment operator: {:?}",
                                    assign_stmt.token
                                );
                                Some(WasmStatement::VarDecl(var_name, value_expr))
                            }
                        }
                    } else {
                        println!("Warning: Skipping complex assignment");
                        None
                    }
                } else {
                    None
                }
            }
            Stmt::Return(return_stmt) => {
                let return_expr = if !return_stmt.results.is_empty() {
                    Some(Self::translate_expression(&return_stmt.results[0], objs))
                } else {
                    None
                };
                Some(WasmStatement::Return(return_expr))
            }
            Stmt::Block(block) => {
                let statements = Self::translate_statements(&block.list, objs);
                Some(WasmStatement::Block(statements))
            }
            Stmt::If(if_stmt) => {
                let condition = Self::translate_expression(&if_stmt.cond, objs);
                let if_statements = Self::translate_statements(&if_stmt.body.list, objs);
                let else_statements = if let Some(ref else_stmt) = if_stmt.els {
                    match else_stmt {
                        Stmt::Block(else_block) => {
                            Some(Self::translate_statements(&else_block.list, objs))
                        }
                        _ => Some(vec![Self::translate_statement_optional(else_stmt, objs)?]),
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
pub struct WasmCompiler {
    types: TypeSection,
    functions: FunctionSection,
    exports: ExportSection,
    codes: CodeSection,
    function_types: HashMap<String, u32>,
    function_indices: HashMap<String, u32>,
    variables: HashMap<String, u32>,
    next_local_index: u32,
    current_func_index: u32,
}

impl WasmCompiler {
    pub fn new() -> Self {
        Self {
            types: TypeSection::new(),
            functions: FunctionSection::new(),
            exports: ExportSection::new(),
            codes: CodeSection::new(),
            function_types: HashMap::new(),
            function_indices: HashMap::new(),
            variables: HashMap::new(),
            next_local_index: 0,
            current_func_index: 0,
        }
    }

    pub fn compile_program(&mut self, program: &WasmProgram) -> Vec<u8> {
        // First pass: register all function signatures
        for func in &program.functions {
            let param_types: Vec<ValType> = func
                .params
                .iter()
                .map(|(_, ty)| match ty {
                    WasmType::Int => ValType::I32,
                    WasmType::Float => ValType::F32,
                    WasmType::Void => panic!("Void cannot be a parameter type"),
                })
                .collect();

            let return_types: Vec<ValType> = match &func.return_type {
                Some(WasmType::Int) => vec![ValType::I32],
                Some(WasmType::Float) => vec![ValType::F32],
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

        // Build the final module
        let mut module = WasmModule::new();
        module.section(&self.types);
        module.section(&self.functions);
        module.section(&self.exports);
        module.section(&self.codes);

        module.finish()
    }

    fn compile_function(&mut self, func: &WasmFunctionDef) {
        // Reset state for this function
        self.variables.clear();
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

        // Only export functions that have //export comments
        if let Some(export_name) = &func.export_name {
            self.exports
                .export(export_name, ExportKind::Func, self.current_func_index);
        }

        // First pass: collect all local variable declarations
        self.collect_variable_declarations(&func.body, &mut locals);

        // Compile function body with correct locals
        let mut f = Function::new(locals);

        // Reset next_local_index to account for parameters only
        self.next_local_index = func.params.len() as u32;

        // Second pass: compile statements with correct local indexing
        for stmt in &func.body {
            self.compile_statement_with_indexing(stmt, &mut f);
        }

        // Ensure function ends properly
        f.instruction(&Instruction::End);
        self.codes.function(&f);

        self.current_func_index += 1;
    }

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
                _ => {}
            }
        }
    }

    fn compile_statement_with_indexing(&mut self, stmt: &WasmStatement, f: &mut Function) {
        match stmt {
            WasmStatement::ExprStmt(expr) => {
                self.compile_expression(expr, f, &mut Vec::new());
                match expr {
                    WasmExpr::Assign(..) => {}
                    _ => {
                        f.instruction(&Instruction::Drop);
                    }
                }
            }
            WasmStatement::VarDecl(name, init_expr) => {
                self.compile_expression(init_expr, f, &mut Vec::new());
                let local_idx = self.next_local_index;
                self.variables.insert(name.clone(), local_idx);
                self.next_local_index += 1;
                f.instruction(&Instruction::LocalSet(local_idx));
            }
            WasmStatement::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.compile_expression(expr, f, &mut Vec::new());
                }
                f.instruction(&Instruction::Return);
            }
            WasmStatement::Block(stmts) => {
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
                f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
                for stmt in body_stmts {
                    self.compile_statement_with_indexing(stmt, f);
                }
                f.instruction(&Instruction::End);
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
            WasmExpr::Variable(name) => {
                if let Some(&idx) = self.variables.get(name) {
                    f.instruction(&Instruction::LocalGet(idx));
                } else {
                    panic!("Undefined variable: {}", name);
                }
            }
            WasmExpr::Binary(op, left, right) => {
                self.compile_expression(left, f, _locals);
                self.compile_expression(right, f, _locals);
                match op {
                    WasmBinaryOp::Add => f.instruction(&Instruction::I32Add),
                    WasmBinaryOp::Sub => f.instruction(&Instruction::I32Sub),
                    WasmBinaryOp::Mul => f.instruction(&Instruction::I32Mul),
                    WasmBinaryOp::Div => f.instruction(&Instruction::I32DivS),
                    WasmBinaryOp::Lt => f.instruction(&Instruction::I32LtS),
                    WasmBinaryOp::Gt => f.instruction(&Instruction::I32GtS),
                    WasmBinaryOp::LtEq => f.instruction(&Instruction::I32LeS),
                    WasmBinaryOp::GtEq => f.instruction(&Instruction::I32GeS),
                    WasmBinaryOp::Eq => f.instruction(&Instruction::I32Eq),
                    WasmBinaryOp::NotEq => f.instruction(&Instruction::I32Ne),
                };
            }
            WasmExpr::Call(func_name, args) => {
                for arg in args {
                    self.compile_expression(arg, f, _locals);
                }
                if let Some(&type_idx) = self.function_types.get(func_name) {
                    f.instruction(&Instruction::Call(type_idx));
                } else {
                    panic!("Undefined function: {}", func_name);
                }
            }
            WasmExpr::Assign(name, value) => {
                self.compile_expression(value, f, _locals);
                if let Some(&idx) = self.variables.get(name) {
                    f.instruction(&Instruction::LocalTee(idx));
                } else {
                    panic!("Undefined variable for assignment: {}", name);
                }
            }
            WasmExpr::Unary(op, operand) => match op {
                WasmUnaryOp::Neg => {
                    f.instruction(&Instruction::I32Const(0));
                    self.compile_expression(operand, f, _locals);
                    f.instruction(&Instruction::I32Sub);
                }
                WasmUnaryOp::Not => {
                    self.compile_expression(operand, f, _locals);
                    f.instruction(&Instruction::I32Eqz);
                }
            },
        }
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
}
