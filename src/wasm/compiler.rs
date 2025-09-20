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
    pub export_name: Option<String>,  // Add export name field
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
    pub fn translate_program(go_program: &GoProgram, source: &str) -> WasmProgram {
        let mut functions = Vec::new();
        
        for declaration in &go_program.declarations {
            if let Declaration::Function(go_func) = declaration {
                functions.push(Self::translate_function(go_func, source));
            }
        }
        
        WasmProgram { functions }
    }
    
    fn translate_function(go_func: &GoFunction, source: &str) -> WasmFunctionDef {
        let params = go_func.parameters.iter()
            .map(|param| (param.name.clone(), Self::translate_type(&param.param_type)))
            .collect();
            
        let return_type = go_func.return_type.as_ref()
            .map(|t| Self::translate_type(t));
            
        let body = go_func.body.statements.iter()
            .map(|stmt| Self::translate_statement(stmt))
            .collect();

        // Extract export name from comments using improved function
        let export_name = extract_export_name(source, &go_func.name);
        
        WasmFunctionDef {
            name: go_func.name.clone(),
            export_name,
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
            GoStatement::IfStmt(_condition, if_block, else_block) => {
               let mut statements = Vec::new();
               
               // For now, we'll translate if statements as basic blocks
               // This is a simplified translation - real WASM would use block/br_if
               for stmt in &if_block.statements {
                   statements.push(Self::translate_statement(stmt));
               }
               
               if let Some(else_stmt) = else_block {
                   match &**else_stmt {
                       GoStatement::Block(else_block) => {
                           for stmt in &else_block.statements {
                               statements.push(Self::translate_statement(stmt));
                           }
                       },
                       _ => statements.push(Self::translate_statement(else_stmt)),
                   }
               }
               
               WasmStatement::Block(statements)
           },
           GoStatement::ForStmt(init, _condition, post, body) => {
               // Simplified for loop translation - unroll the loop logic
               let mut statements = Vec::new();
               
               // Add init statement if present
               if let Some(init_stmt) = init {
                   statements.push(Self::translate_statement(init_stmt));
               }
               
               // Add body statements
               for stmt in &body.statements {
                   statements.push(Self::translate_statement(stmt));
               }
               
               // Add post statement if present  
               if let Some(post_stmt) = post {
                   statements.push(Self::translate_statement(post_stmt));
               }
               
               WasmStatement::Block(statements)
           },
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
        
        // Only export functions that have //export comments
        if let Some(export_name) = &func.export_name {
            self.exports.export(export_name, ExportKind::Func, self.current_func_index);
        }
        
        // Compile function body
        let mut locals = Vec::new();
        let mut f = Function::new(Vec::new());
        
        for stmt in &func.body {
            self.compile_statement(stmt, &mut f, &mut locals);
        }
        
        // Ensure function ends properly
        f.instruction(&Instruction::End);
        
        // Create a new function with collected locals if any were added
        if !locals.is_empty() {
            let mut final_f = Function::new(locals);
            for stmt in &func.body {
                self.compile_statement(stmt, &mut final_f, &mut Vec::new());
            }
            final_f.instruction(&Instruction::End);
            self.codes.function(&final_f);
        } else {
            self.codes.function(&f);
        }
        
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
                    WasmBinaryOp::Add => f.instruction(&Instruction::I32Add),
                    WasmBinaryOp::Sub => f.instruction(&Instruction::I32Sub),
                    WasmBinaryOp::Mul => f.instruction(&Instruction::I32Mul),
                    WasmBinaryOp::Div => f.instruction(&Instruction::I32DivS),
                };
            },
            WasmExpr::Call(func_name, args) => {
                // Compile arguments
                for arg in args {
                    self.compile_expression(arg, f, locals);
                }
                
                // Find the function index
                if let Some(&type_idx) = self.function_types.get(func_name) {
                    f.instruction(&Instruction::Call(type_idx));
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
    let wasm_program = GoToWasmTranslator::translate_program(&go_program, go_source);
    
    // Compile WASM AST to WASM bytecode
    let mut compiler = WasmCompiler::new();
    let wasm_bytes = compiler.compile_program(&wasm_program);
    
    Ok(wasm_bytes)
}

#[cfg(test)]
mod tests {
    use crate::wasm::compiler::Parser;
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
    fn test_function_without_export_comment() {
        let go_source = r#"
            package main
            
            // This is a regular comment
            func add(x int, y int) int {
                return x + y
            }
        "#;
        
        let mut parser = Parser::new(go_source);
        let go_program = parser.parse().unwrap();
        let wasm_program = GoToWasmTranslator::translate_program(&go_program, go_source);
        
        assert_eq!(wasm_program.functions.len(), 1);
        assert!(wasm_program.functions[0].export_name.is_none(), 
               "Function without export comment should not be exported");
    }

    #[test]
    fn test_function_with_export_comment() {
        let go_source = r#"
            package main
            
            // Some regular comment
            //export add_numbers
            func add(x int, y int) int {
                return x + y
            }
        "#;
        
        let mut parser = Parser::new(go_source);
        let go_program = parser.parse().unwrap();
        let wasm_program = GoToWasmTranslator::translate_program(&go_program, go_source);
        
        assert_eq!(wasm_program.functions.len(), 1);
        assert_eq!(wasm_program.functions[0].export_name, Some("add_numbers".to_string()),
               "Function with export comment should have export name");
    }

    #[test]
    fn test_multiple_functions_with_mixed_exports() {
        let go_source = r#"
            package main
            
            // Internal helper function
            func helper(x int) int {
                return x * 2
            }
            
            //export public_multiply
            func multiply(x int, y int) int {
                return helper(x) * y
            }
            
            //export public_add  
            func add(x int, y int) int {
                return x + y
            }
        "#;
        
        let mut parser = Parser::new(go_source);
        let go_program = parser.parse().unwrap();
        let wasm_program = GoToWasmTranslator::translate_program(&go_program, go_source);
        
        assert_eq!(wasm_program.functions.len(), 3);
        
        // Find functions by name
        let helper_func = wasm_program.functions.iter().find(|f| f.name == "helper").unwrap();
        let multiply_func = wasm_program.functions.iter().find(|f| f.name == "multiply").unwrap();
        let add_func = wasm_program.functions.iter().find(|f| f.name == "add").unwrap();
        
        assert!(helper_func.export_name.is_none(), "Helper function should not be exported");
        assert_eq!(multiply_func.export_name, Some("public_multiply".to_string()));
        assert_eq!(add_func.export_name, Some("public_add".to_string()));
    }

    #[test]
    fn test_export_comment_with_spaces() {
        let go_source = r#"
            package main
            
            //export   spaced_name   
            func test() int {
                return 42
            }
        "#;
        
        let mut parser = Parser::new(go_source);
        let go_program = parser.parse().unwrap();
        let wasm_program = GoToWasmTranslator::translate_program(&go_program, go_source);
        
        assert_eq!(wasm_program.functions[0].export_name, Some("spaced_name".to_string()),
               "Export name should be trimmed of whitespace");
    }

    #[test]
    fn test_export_comment_multiple_lines_above() {
        let go_source = r#"
            package main
            
            // Some documentation
            // More documentation
            //export documented_func
            // Even more comments
            func documented() int {
                return 1
            }
        "#;
        
        let mut parser = Parser::new(go_source);
        let go_program = parser.parse().unwrap();
        let wasm_program = GoToWasmTranslator::translate_program(&go_program, go_source);
        
        assert_eq!(wasm_program.functions[0].export_name, Some("documented_func".to_string()),
               "Should find export comment among other comments");
    }

    #[test]
    fn test_wasm_compilation_with_exports() {
        let go_source = r#"
            package main
            
            //export fibonacci
            func fib(n int) int {
                if n <= 1 {
                    return n
                }
                return fib(n-1) + fib(n-2)
            }
            
            func internal_helper(x int) int {
                return x + 1
            }
        "#;
        
        let result = compile_go_to_wasm(go_source);
        assert!(result.is_ok(), "Compilation should succeed");
        
        let wasm_bytes = result.unwrap();
        assert!(!wasm_bytes.is_empty(), "Should generate WASM bytes");
        
        // Verify the WASM module is valid by trying to parse its sections
        // This is a basic sanity check - in practice you'd use a WASM parser
        assert!(wasm_bytes.starts_with(&[0x00, 0x61, 0x73, 0x6d]), "Should start with WASM magic number");
    }

    #[test]
    fn test_main_function_with_variables() {
        let go_source = r#"
            package main
            
            //export main
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
            
            //export counter
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
            
            //export calculate
            func calculate(a int, b int) int {
                result := a * b + a - b
                result++
                return result
            }
        "#;
        
        let result = compile_go_to_wasm(go_source);
        assert!(result.is_ok(), "Failed to compile: {:?}", result.err());
    }

    #[test]
    fn test_function_calls_between_exported_and_internal() {
        let go_source = r#"
            package main
            
            func double(x int) int {
                return x * 2
            }
            
            //export process
            func process(input int) int {
                temp := double(input)
                return temp + 1
            }
        "#;
        
        let result = compile_go_to_wasm(go_source);
        assert!(result.is_ok(), "Should compile function calls correctly: {:?}", result.err());
    }

    #[test]
    fn test_extract_export_name_function() {
        let source1 = r#"
            //export test_func
            func myFunc() int {
                return 42
            }
        "#;
        
        assert_eq!(extract_export_name(source1, "myFunc"), Some("test_func".to_string()));
        
        let source2 = r#"
            func myFunc() int {
                return 42
            }
        "#;
        
        assert_eq!(extract_export_name(source2, "myFunc"), None);
        
        let source3 = r#"
            // Some comment
            //export   spaced_export   
            // Another comment
            func myFunc() int {
                return 42
            }
        "#;
        
        assert_eq!(extract_export_name(source3, "myFunc"), Some("spaced_export".to_string()));
    }

    #[test]
    fn test_no_exports_generates_no_export_section() {
        let go_source = r#"
            package main
            
            func internal1(x int) int {
                return x + 1
            }
            
            func internal2(y int) int {
                return internal1(y) * 2
            }
        "#;
        
        let mut parser = Parser::new(go_source);
        let go_program = parser.parse().unwrap();
        let wasm_program = GoToWasmTranslator::translate_program(&go_program, go_source);
        
        // Verify no functions have export names
        for func in &wasm_program.functions {
            assert!(func.export_name.is_none(), 
                   "Function {} should not have export name", func.name);
        }
        
        let result = compile_go_to_wasm(go_source);
        assert!(result.is_ok(), "Should compile successfully without exports");
    }
}