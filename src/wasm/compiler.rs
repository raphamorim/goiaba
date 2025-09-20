// Copyright 2025 Raphael Amorim. All rights reserved.
// Use of this source code is governed by a GPL-3.0
// license that can be found in the LICENSE file.

use std::collections::HashMap;
use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, Function, FunctionSection, 
    Instruction, Module as WasmModule, TypeSection, ValType,
};

// Import the Go parser types
use crate::parser::{Program as GoProgram, Parser, BinaryOp, Declaration, Function as GoFunction, Statement as GoStatement, Expression as GoExpression};

// Define our WASM-specific AST structures
#[derive(Debug, Clone)]
pub enum WasmExpr {
    Integer(i32),
    Variable(String),
    Binary(WasmBinaryOp, Box<WasmExpr>, Box<WasmExpr>),
    Call(String, Vec<WasmExpr>),
    Assign(String, Box<WasmExpr>),
}

#[derive(Debug, Clone)]
pub enum WasmBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
pub enum WasmStatement {
    ExprStmt(WasmExpr),
    VarDecl(String, WasmExpr),
    Return(Option<WasmExpr>),
    Block(Vec<WasmStatement>),
}

#[derive(Debug, Clone)]
pub struct WasmFunctionDef {
    pub name: String,
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

impl GoToWasmTranslator {
    pub fn translate_program(go_program: &GoProgram) -> WasmProgram {
        let mut functions = Vec::new();
        
        for declaration in &go_program.declarations {
            if let Declaration::Function(go_func) = declaration {
                functions.push(Self::translate_function(go_func));
            }
        }
        
        WasmProgram { functions }
    }
    
    fn translate_function(go_func: &GoFunction) -> WasmFunctionDef {
        let params = go_func.parameters.iter()
            .map(|param| (param.name.clone(), Self::translate_type(&param.param_type)))
            .collect();
            
        let return_type = go_func.return_type.as_ref()
            .map(|t| Self::translate_type(t));
            
        let body = go_func.body.statements.iter()
            .map(|stmt| Self::translate_statement(stmt))
            .collect();
        
        WasmFunctionDef {
            name: go_func.name.clone(),
            params,
            return_type,
            body,
        }
    }
    
    fn translate_statement(go_stmt: &GoStatement) -> WasmStatement {
        match go_stmt {
            GoStatement::ExpressionStmt(expr) => {
                WasmStatement::ExprStmt(Self::translate_expression(expr))
            },
            GoStatement::ShortDeclStmt(names, values) => {
                // For simplicity, handle only single variable declarations
                if names.len() == 1 && values.len() == 1 {
                    WasmStatement::VarDecl(
                        names[0].clone(),
                        Self::translate_expression(&values[0])
                    )
                } else {
                    panic!("Multiple variable declarations not yet supported");
                }
            },
            GoStatement::ReturnStmt(expr_opt) => {
                WasmStatement::Return(expr_opt.as_ref().map(|e| Self::translate_expression(e)))
            },
            GoStatement::Block(block) => {
                let statements = block.statements.iter()
                    .map(|s| Self::translate_statement(s))
                    .collect();
                WasmStatement::Block(statements)
            },
            GoStatement::AssignmentStmt(left_exprs, right_exprs) => {
                // Handle simple single assignment
                if left_exprs.len() == 1 && right_exprs.len() == 1 {
                    if let GoExpression::Identifier(name) = &left_exprs[0] {
                        WasmStatement::ExprStmt(WasmExpr::Assign(
                            name.clone(),
                            Box::new(Self::translate_expression(&right_exprs[0]))
                        ))
                    } else {
                        panic!("Complex left-hand assignments not supported");
                    }
                } else {
                    panic!("Multiple assignments not yet supported");
                }
            },
            GoStatement::IncrementStmt(expr) => {
                if let GoExpression::Identifier(name) = expr {
                    // Translate i++ to i = i + 1
                    WasmStatement::ExprStmt(WasmExpr::Assign(
                        name.clone(),
                        Box::new(WasmExpr::Binary(
                            WasmBinaryOp::Add,
                            Box::new(WasmExpr::Variable(name.clone())),
                            Box::new(WasmExpr::Integer(1))
                        ))
                    ))
                } else {
                    panic!("Increment on complex expressions not supported");
                }
            },
            GoStatement::DecrementStmt(expr) => {
                if let GoExpression::Identifier(name) = expr {
                    // Translate i-- to i = i - 1
                    WasmStatement::ExprStmt(WasmExpr::Assign(
                        name.clone(),
                        Box::new(WasmExpr::Binary(
                            WasmBinaryOp::Sub,
                            Box::new(WasmExpr::Variable(name.clone())),
                            Box::new(WasmExpr::Integer(1))
                        ))
                    ))
                } else {
                    panic!("Decrement on complex expressions not supported");
                }
            },
            _ => panic!("Statement type not yet supported: {:?}", go_stmt),
        }
    }
    
    fn translate_expression(go_expr: &GoExpression) -> WasmExpr {
        match go_expr {
            GoExpression::IntLiteral(value) => WasmExpr::Integer(*value as i32),
            GoExpression::Identifier(name) => WasmExpr::Variable(name.clone()),
            GoExpression::BinaryExpr(left, op, right) => {
                WasmExpr::Binary(
                    Self::translate_binary_op(op),
                    Box::new(Self::translate_expression(left)),
                    Box::new(Self::translate_expression(right))
                )
            },
            GoExpression::CallExpr(func_expr, args) => {
                if let GoExpression::Identifier(func_name) = &**func_expr {
                    let wasm_args = args.iter()
                        .map(|arg| Self::translate_expression(arg))
                        .collect();
                    WasmExpr::Call(func_name.clone(), wasm_args)
                } else {
                    panic!("Complex function calls not yet supported");
                }
            },
            _ => panic!("Expression type not yet supported: {:?}", go_expr),
        }
    }
    
    fn translate_binary_op(go_op: &BinaryOp) -> WasmBinaryOp {
        match go_op {
            BinaryOp::Add => WasmBinaryOp::Add,
            BinaryOp::Sub => WasmBinaryOp::Sub,
            BinaryOp::Mul => WasmBinaryOp::Mul,
            BinaryOp::Div => WasmBinaryOp::Div,
            _ => panic!("Binary operator not yet supported: {:?}", go_op),
        }
    }
    
    fn translate_type(type_str: &str) -> WasmType {
        match type_str {
            "int" => WasmType::Int,
            "float32" | "float64" => WasmType::Float,
            _ => WasmType::Int, // Default to int for unknown types
        }
    }
}

// WASM Compiler
pub struct WasmCompiler {
    types: TypeSection,
    functions: FunctionSection,
    exports: ExportSection,
    codes: CodeSection,
    function_types: HashMap<String, u32>,
    function_indices: HashMap<String, u32>, // Maps to function index
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
            let param_types: Vec<ValType> = func.params.iter()
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
            self.function_indices.insert(func.name.clone(), self.current_func_index);
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
        for (i, (name, _)) in func.params.iter().enumerate() {
            self.variables.insert(name.clone(), i as u32);
            self.next_local_index += 1;
        }
        
        // Get function type index
        let type_index = *self.function_types.get(&func.name).unwrap();
        
        // Add function to function section
        self.functions.function(type_index);
        
        // Export the function (export main functions and public functions)
        if func.name == "main" || !func.name.starts_with('_') {
            self.exports.export(&func.name, ExportKind::Func, self.current_func_index);
        }
        
        // Compile function body
        let mut locals = Vec::new();
        let mut f = Function::new(locals.clone());
        
        for stmt in &func.body {
            self.compile_statement(stmt, &mut f, &mut locals);
        }
        
        // Ensure function ends properly
        f.instruction(&Instruction::End);
        
        // Add any additional locals that were created
        if !locals.is_empty() {
            f = Function::new(locals);
            for stmt in &func.body {
                self.compile_statement(stmt, &mut f, &mut Vec::new());
            }
            f.instruction(&Instruction::End);
        }
        
        self.codes.function(&f);
        self.current_func_index += 1;
    }
    
    fn compile_statement(&mut self, stmt: &WasmStatement, f: &mut Function, locals: &mut Vec<(u32, ValType)>) {
        match stmt {
            WasmStatement::ExprStmt(expr) => {
                self.compile_expression(expr, f, locals);
                // For expression statements, discard the result if it produces one
                match expr {
                    WasmExpr::Assign(..) => {}, // Assignment returns void
                    _ => { f.instruction(&Instruction::Drop); },
                }
            },
            WasmStatement::VarDecl(name, init_expr) => {
                // Compile the initializer expression
                self.compile_expression(init_expr, f, locals);
                
                // Add a new local variable
                let local_idx = self.next_local_index;
                self.variables.insert(name.clone(), local_idx);
                locals.push((1, ValType::I32)); // Assuming all variables are i32 for simplicity
                self.next_local_index += 1;
                
                // Store the value in the local
                f.instruction(&Instruction::LocalSet(local_idx));
            },
            WasmStatement::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.compile_expression(expr, f, locals);
                }
                f.instruction(&Instruction::Return);
            },
            WasmStatement::Block(stmts) => {
                for s in stmts {
                    self.compile_statement(s, f, locals);
                }
            },
        }
    }
    
    fn compile_expression(&mut self, expr: &WasmExpr, f: &mut Function, locals: &mut Vec<(u32, ValType)>) {
        match expr {
            WasmExpr::Integer(value) => {
                 f.instruction(&Instruction::I32Const(*value));
            },
            WasmExpr::Variable(name) => {
                if let Some(&idx) = self.variables.get(name) {
                    f.instruction(&Instruction::LocalGet(idx));
                } else {
                    panic!("Undefined variable: {}", name);
                }
            },
            WasmExpr::Binary(op, left, right) => {
                self.compile_expression(left, f, locals);
                self.compile_expression(right, f, locals);
                
                match op {
                    WasmBinaryOp::Add => { f.instruction(&Instruction::I32Add); },
                    WasmBinaryOp::Sub => { f.instruction(&Instruction::I32Sub); },
                    WasmBinaryOp::Mul => { f.instruction(&Instruction::I32Mul); },
                    WasmBinaryOp::Div => { f.instruction(&Instruction::I32DivS); },
                }
            },
            WasmExpr::Call(func_name, args) => {
                // Compile arguments
                for arg in args {
                    self.compile_expression(arg, f, locals);
                }
                
                // Find the function index
                if let Some(&func_idx) = self.function_indices.get(func_name) {
                    f.instruction(&Instruction::Call(func_idx));
                } else {
                    panic!("Undefined function: {}", func_name);
                }
            },
            WasmExpr::Assign(name, value) => {
                self.compile_expression(value, f, locals);
                
                if let Some(&idx) = self.variables.get(name) {
                    // Use LocalTee to store the value and leave a copy on the stack
                    f.instruction(&Instruction::LocalTee(idx));
                } else {
                    panic!("Undefined variable for assignment: {}", name);
                }
            },
        }
    }
}

// High-level function to compile Go source to WASM
pub fn compile_go_to_wasm(go_source: &str) -> Result<Vec<u8>, String> {
    // Parse the Go source code
    let mut parser = Parser::new(go_source);
    let go_program = parser.parse()?;
    
    // Translate Go AST to WASM AST
    let wasm_program = GoToWasmTranslator::translate_program(&go_program);
    
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
        
        let result = compile_go_to_wasm(go_source);
        assert!(result.is_ok(), "Failed to compile: {:?}", result.err());
        
        let wasm_bytes = result.unwrap();
        assert!(!wasm_bytes.is_empty(), "Generated WASM should not be empty");
    }

    #[test]
    fn test_main_function_with_variables() {
        let go_source = r#"
            package main
            
            func main() {
                x := 42
                y := x + 10
                return y
            }
        "#;
        
        let result = compile_go_to_wasm(go_source);
        assert!(result.is_ok(), "Failed to compile: {:?}", result.err());
    }

    #[test]
    fn test_function_with_increment() {
        let go_source = r#"
            package main
            
            func counter() int {
                i := 0
                i++
                return i
            }
        "#;
        
        let result = compile_go_to_wasm(go_source);
        assert!(result.is_ok(), "Failed to compile: {:?}", result.err());
    }

    #[test]
    fn test_complex_arithmetic() {
        let go_source = r#"
            package main
            
            func calculate(a int, b int) int {
                result := a * b + a - b
                return result
            }
        "#;
        
        let result = compile_go_to_wasm(go_source);
        assert!(result.is_ok(), "Failed to compile: {:?}", result.err());
    }
}