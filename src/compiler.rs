// Copyright 2025 Raphael Amorim. All rights reserved.
// Use of this source code is governed by a GPL-3.0
// license that can be found in the LICENSE file.

use std::collections::HashMap;
use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, FunctionSection, GlobalSection, 
    Instruction, Module as WasmModule, TypeSection, ValType,
};

// Define our AST structures
#[derive(Debug, Clone)]
pub enum Expr {
    Integer(i32),
    Variable(String),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Assign(String, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
pub enum Statement {
    ExprStmt(Expr),
    VarDecl(String, Expr),
    Return(Option<Expr>),
    Block(Vec<Statement>),
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_type: Option<Type>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Void,
}

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<FunctionDef>,
}

// Compiler module
pub mod compiler {
    use super::*;

    pub struct Compiler {
        types: TypeSection,
        functions: FunctionSection,
        exports: ExportSection,
        globals: GlobalSection,
        codes: CodeSection,
        function_types: HashMap<String, (u32, Vec<ValType>, Vec<ValType>)>,
        variables: HashMap<String, usize>,
        next_local_index: usize,
        current_func_index: usize,
    }

    impl Compiler {
        pub fn new() -> Self {
            Self {
                types: TypeSection::new(),
                functions: FunctionSection::new(),
                exports: ExportSection::new(),
                globals: GlobalSection::new(),
                codes: CodeSection::new(),
                function_types: HashMap::new(),
                variables: HashMap::new(),
                next_local_index: 0,
                current_func_index: 0,
            }
        }

        pub fn compile_program(&mut self, program: &Program) -> Vec<u8> {
            // First pass: register all function signatures
            for func in program.functions.iter() {
                let param_types = func.params.iter()
                    .map(|(_, ty)| match ty {
                        Type::Int => ValType::I32,
                        Type::Float => ValType::F32,
                        Type::Void => panic!("Void cannot be a parameter type"),
                    })
                    .collect::<Vec<_>>();
                
                let return_types = match &func.return_type {
                    Some(Type::Int) => vec![ValType::I32],
                    Some(Type::Float) => vec![ValType::F32],
                    Some(Type::Void) | None => vec![],
                };
                
                let type_index = self.types.len() as u32;
                // self.types.function(param_types.clone(), return_types.clone());
                self.function_types.insert(func.name.clone(), (type_index, param_types, return_types));
            }
            
            // Second pass: compile function bodies
            for func in &program.functions {
                self.compile_function(func);
            }
            
            // Build the final module
            let mut module = WasmModule::new();
            module.section(&self.types);
            module.section(&self.functions);
            
            if !self.globals.is_empty() {
                module.section(&self.globals);
            }
            
            module.section(&self.exports);
            module.section(&self.codes);
            
            module.finish()
        }
        
        fn compile_function(&mut self, func: &FunctionDef) {
            // Reset state for this function
            self.variables.clear();
            self.next_local_index = 0;
            
            // Register parameters as local variables
            for (i, (name, _)) in func.params.iter().enumerate() {
                self.variables.insert(name.clone(), i);
                self.next_local_index += 1;
            }
            
            // Get function type index
            let (type_index, _, _) = self.function_types.get(&func.name).unwrap().clone();
            
            // Add function to function section
            self.functions.function(type_index);
            
            // Export the function
            self.exports.export(&func.name, ExportKind::Func, self.current_func_index as u32);
            
            // Compile function body
            let mut locals = Vec::new();
            let mut instructions = Vec::new();
            
            for stmt in &func.body {
                self.compile_statement(stmt, &mut instructions, &mut locals);
            }
            
            // Encode the code section.
            // let mut codes = CodeSection::new();
            // let locals = vec![];
            // let mut f = Function::new(locals);
            // f.instructions()
            //     .local_get(0)
            //     .local_get(1)
            //     .i32_add()
            //     .end();
            
            // Ensure the function ends properly
            // if !instructions.ends_with(&[Instruction::End]) {
            //     instructions.push(Instruction::End);
            // }
            
            // Create function body
            // self.codes.function(&locals);
            
            self.current_func_index += 1;
        }
        
        fn compile_statement(&mut self, stmt: &Statement, instructions: &mut Vec<Instruction>, locals: &mut Vec<(u32, ValType)>) {
            match stmt {
                Statement::ExprStmt(expr) => {
                    self.compile_expression(expr, instructions, locals);
                    // For expression statements, discard the result
                    instructions.push(Instruction::Drop);
                },
                Statement::VarDecl(name, init_expr) => {
                    // Compile the initializer expression
                    self.compile_expression(init_expr, instructions, locals);
                    
                    // Add a new local variable
                    let local_idx = self.next_local_index;
                    self.variables.insert(name.clone(), local_idx);
                    locals.push((1, ValType::I32)); // Assuming all variables are i32 for simplicity
                    self.next_local_index += 1;
                    
                    // Store the value in the local
                    instructions.push(Instruction::LocalSet(local_idx as u32));
                },
                Statement::Return(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        self.compile_expression(expr, instructions, locals);
                        instructions.push(Instruction::Return);
                    } else {
                        instructions.push(Instruction::Return);
                    }
                },
                Statement::Block(stmts) => {
                    for s in stmts {
                        self.compile_statement(s, instructions, locals);
                    }
                },
            }
        }
        
        fn compile_expression(&mut self, expr: &Expr, instructions: &mut Vec<Instruction>, locals: &mut Vec<(u32, ValType)>) {
            match expr {
                Expr::Integer(value) => {
                    instructions.push(Instruction::I32Const(*value));
                },
                Expr::Variable(name) => {
                    if let Some(&idx) = self.variables.get(name) {
                        instructions.push(Instruction::LocalGet(idx as u32));
                    } else {
                        panic!("Undefined variable: {}", name);
                    }
                },
                Expr::Binary(op, left, right) => {
                    self.compile_expression(left, instructions, locals);
                    self.compile_expression(right, instructions, locals);
                    
                    match op {
                        BinaryOp::Add => instructions.push(Instruction::I32Add),
                        BinaryOp::Sub => instructions.push(Instruction::I32Sub),
                        BinaryOp::Mul => instructions.push(Instruction::I32Mul),
                        BinaryOp::Div => instructions.push(Instruction::I32DivS),
                    }
                },
                Expr::Call(func_name, args) => {
                    // Compile arguments
                    for arg in args {
                        self.compile_expression(arg, instructions, locals);
                    }
                    
                    // Find the function index
                    let mut found = false;
                    for (i, (name, _)) in self.function_types.iter().enumerate() {
                        if name == func_name {
                            instructions.push(Instruction::Call(i as u32));
                            found = true;
                            break;
                        }
                    }
                    
                    if !found {
                        panic!("Undefined function: {}", func_name);
                    }
                },
                Expr::Assign(name, value) => {
                    self.compile_expression(value, instructions, locals);
                    
                    if let Some(&idx) = self.variables.get(name) {
                        // Duplicate the value for assignment expression result
                        instructions.push(Instruction::LocalTee(idx as u32));
                    } else {
                        panic!("Undefined variable for assignment: {}", name);
                    }
                },
            }
        }
    }
}

// Example usage
fn main() {
    use compiler::Compiler;
    
    // Create a simple program: function add(a: int, b: int) -> int { return a + b; }
    let add_func = FunctionDef {
        name: "add".to_string(),
        params: vec![
            ("a".to_string(), Type::Int),
            ("b".to_string(), Type::Int),
        ],
        return_type: Some(Type::Int),
        body: vec![
            Statement::Return(Some(
                Expr::Binary(
                    BinaryOp::Add,
                    Box::new(Expr::Variable("a".to_string())),
                    Box::new(Expr::Variable("b".to_string())),
                )
            )),
        ],
    };
    
    // Create a factorial function (simplified without loops)
    let factorial_func = FunctionDef {
        name: "factorial".to_string(),
        params: vec![("n".to_string(), Type::Int)],
        return_type: Some(Type::Int),
        body: vec![
            Statement::VarDecl("result".to_string(), Expr::Integer(1)),
            Statement::ExprStmt(Expr::Assign(
                "result".to_string(),
                Box::new(Expr::Binary(
                    BinaryOp::Mul,
                    Box::new(Expr::Variable("result".to_string())),
                    Box::new(Expr::Variable("n".to_string())),
                )),
            )),
            Statement::Return(Some(Expr::Variable("result".to_string()))),
        ],
    };
    
    let program = Program {
        functions: vec![add_func, factorial_func],
    };
    
    let mut compiler = Compiler::new();
    let wasm_bytes = compiler.compile_program(&program);
    
    // Save the WebAssembly binary to a file
    std::fs::write("output.wasm", &wasm_bytes).unwrap();
    println!("WebAssembly module written to output.wasm");
}