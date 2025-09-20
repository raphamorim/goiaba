// Copyright 2025 Raphael Amorim. All rights reserved.
// Use of this source code is governed by a GPL-3.0
// license that can be found in the LICENSE file.

use std::collections::HashMap;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, PartialEq)]
enum TokenType {
    // Keywords
    Package,
    Import,
    Func,
    Return,
    Var,
    Const,
    If,
    Else,
    For,
    Break,
    Continue,
    Type,
    Struct,
    Interface,

    // Literals
    Identifier,
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    CharLiteral,

    // Operators
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    And,
    Or,
    Not,
    Assign,
    ShortDeclare,

    // Delimiters
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Period,
    Semicolon,
    Colon,

    // End of file
    EOF,
}

#[derive(Debug, Clone)]
struct Token {
    token_type: TokenType,
    lexeme: String,
    position: usize,
}

struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    position: usize,
    keywords: HashMap<String, TokenType>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        let mut keywords = HashMap::new();
        keywords.insert("package".to_string(), TokenType::Package);
        keywords.insert("import".to_string(), TokenType::Import);
        keywords.insert("func".to_string(), TokenType::Func);
        keywords.insert("return".to_string(), TokenType::Return);
        keywords.insert("var".to_string(), TokenType::Var);
        keywords.insert("const".to_string(), TokenType::Const);
        keywords.insert("if".to_string(), TokenType::If);
        keywords.insert("else".to_string(), TokenType::Else);
        keywords.insert("for".to_string(), TokenType::For);
        keywords.insert("break".to_string(), TokenType::Break);
        keywords.insert("continue".to_string(), TokenType::Continue);
        keywords.insert("type".to_string(), TokenType::Type);
        keywords.insert("struct".to_string(), TokenType::Struct);
        keywords.insert("interface".to_string(), TokenType::Interface);

        Lexer {
            input: input.chars().peekable(),
            position: 0,
            keywords,
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if let Some(&c) = self.input.peek() {
            let start_pos = self.position;

            return match c {
                'a'..='z' | 'A'..='Z' | '_' => {
                    let identifier = self.read_identifier();
                    let token_type = self
                        .keywords
                        .get(&identifier)
                        .cloned()
                        .unwrap_or(TokenType::Identifier);

                    return Token {
                        token_type,
                        lexeme: identifier,
                        position: start_pos,
                    };
                }
                '0'..='9' => {
                    let number = self.read_number();
                    let token_type = if number.contains('.') {
                        TokenType::FloatLiteral
                    } else {
                        TokenType::IntLiteral
                    };

                    return Token {
                        token_type,
                        lexeme: number,
                        position: start_pos,
                    };
                }
                '"' => {
                    self.input.next();
                    self.position += 1;
                    let string_content = self.read_string();

                    return Token {
                        token_type: TokenType::StringLiteral,
                        lexeme: string_content,
                        position: start_pos,
                    };
                }
                '\'' => {
                    self.input.next();
                    self.position += 1;
                    let char_content = self.read_char();

                    return Token {
                        token_type: TokenType::CharLiteral,
                        lexeme: char_content,
                        position: start_pos,
                    };
                }
                '/' => {
                    self.input.next();
                    self.position += 1;

                    if let Some(&next_char) = self.input.peek() {
                        if next_char == '/' {
                            // Line comment
                            self.skip_line_comment();
                            return self.next_token();
                        } else if next_char == '*' {
                            // Block comment
                            self.skip_block_comment();
                            return self.next_token();
                        }
                    }

                    return Token {
                        token_type: TokenType::Slash,
                        lexeme: "/".to_string(),
                        position: start_pos,
                    };
                }
                '+' => self.create_simple_token(TokenType::Plus, "+"),
                '-' => self.create_simple_token(TokenType::Minus, "-"),
                '*' => self.create_simple_token(TokenType::Asterisk, "*"),
                '%' => self.create_simple_token(TokenType::Percent, "%"),
                '(' => self.create_simple_token(TokenType::LParen, "("),
                ')' => self.create_simple_token(TokenType::RParen, ")"),
                '{' => self.create_simple_token(TokenType::LBrace, "{"),
                '}' => self.create_simple_token(TokenType::RBrace, "}"),
                '[' => self.create_simple_token(TokenType::LBracket, "["),
                ']' => self.create_simple_token(TokenType::RBracket, "]"),
                ',' => self.create_simple_token(TokenType::Comma, ","),
                '.' => self.create_simple_token(TokenType::Period, "."),
                ';' => self.create_simple_token(TokenType::Semicolon, ";"),
                ':' => {
                    self.input.next();
                    self.position += 1;

                    if let Some(&next_char) = self.input.peek() {
                        if next_char == '=' {
                            self.input.next();
                            self.position += 1;
                            return Token {
                                token_type: TokenType::ShortDeclare,
                                lexeme: ":=".to_string(),
                                position: start_pos,
                            };
                        }
                    }

                    return Token {
                        token_type: TokenType::Colon,
                        lexeme: ":".to_string(),
                        position: start_pos,
                    };
                }
                '=' => {
                    self.input.next();
                    self.position += 1;

                    if let Some(&next_char) = self.input.peek() {
                        if next_char == '=' {
                            self.input.next();
                            self.position += 1;
                            return Token {
                                token_type: TokenType::Equal,
                                lexeme: "==".to_string(),
                                position: start_pos,
                            };
                        }
                    }

                    return Token {
                        token_type: TokenType::Assign,
                        lexeme: "=".to_string(),
                        position: start_pos,
                    };
                }
                '!' => {
                    self.input.next();
                    self.position += 1;

                    if let Some(&next_char) = self.input.peek() {
                        if next_char == '=' {
                            self.input.next();
                            self.position += 1;
                            return Token {
                                token_type: TokenType::NotEqual,
                                lexeme: "!=".to_string(),
                                position: start_pos,
                            };
                        }
                    }

                    return Token {
                        token_type: TokenType::Not,
                        lexeme: "!".to_string(),
                        position: start_pos,
                    };
                }
                '<' => {
                    self.input.next();
                    self.position += 1;

                    if let Some(&next_char) = self.input.peek() {
                        if next_char == '=' {
                            self.input.next();
                            self.position += 1;
                            return Token {
                                token_type: TokenType::LessEqual,
                                lexeme: "<=".to_string(),
                                position: start_pos,
                            };
                        }
                    }

                    return Token {
                        token_type: TokenType::LessThan,
                        lexeme: "<".to_string(),
                        position: start_pos,
                    };
                }
                '>' => {
                    self.input.next();
                    self.position += 1;

                    if let Some(&next_char) = self.input.peek() {
                        if next_char == '=' {
                            self.input.next();
                            self.position += 1;
                            return Token {
                                token_type: TokenType::GreaterEqual,
                                lexeme: ">=".to_string(),
                                position: start_pos,
                            };
                        }
                    }

                    return Token {
                        token_type: TokenType::GreaterThan,
                        lexeme: ">".to_string(),
                        position: start_pos,
                    };
                }
                '&' => {
                    self.input.next();
                    self.position += 1;

                    if let Some(&next_char) = self.input.peek() {
                        if next_char == '&' {
                            self.input.next();
                            self.position += 1;
                            return Token {
                                token_type: TokenType::And,
                                lexeme: "&&".to_string(),
                                position: start_pos,
                            };
                        }
                    }

                    // In a full implementation, we would handle the single '&' for references/bitwise ops
                    return Token {
                        token_type: TokenType::And,
                        lexeme: "&".to_string(),
                        position: start_pos,
                    };
                }
                '|' => {
                    self.input.next();
                    self.position += 1;

                    if let Some(&next_char) = self.input.peek() {
                        if next_char == '|' {
                            self.input.next();
                            self.position += 1;
                            return Token {
                                token_type: TokenType::Or,
                                lexeme: "||".to_string(),
                                position: start_pos,
                            };
                        }
                    }

                    // In a full implementation, we would handle the single '|' for bitwise ops
                    return Token {
                        token_type: TokenType::Or,
                        lexeme: "|".to_string(),
                        position: start_pos,
                    };
                }
                _ => {
                    self.input.next();
                    self.position += 1;
                    return self.next_token(); // Skip unrecognized characters
                }
            };
        }

        // End of file
        Token {
            token_type: TokenType::EOF,
            lexeme: "".to_string(),
            position: self.position,
        }
    }

    fn create_simple_token(&mut self, token_type: TokenType, lexeme: &str) -> Token {
        let position = self.position;
        self.input.next();
        self.position += 1;

        Token {
            token_type,
            lexeme: lexeme.to_string(),
            position,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.input.peek() {
            if c.is_whitespace() {
                self.input.next();
                self.position += 1;
            } else {
                break;
            }
        }
    }

    fn skip_line_comment(&mut self) {
        self.input.next(); // Skip the second '/'
        self.position += 1;

        while let Some(&c) = self.input.peek() {
            self.input.next();
            self.position += 1;

            if c == '\n' {
                break;
            }
        }
    }

    fn skip_block_comment(&mut self) {
        self.input.next(); // Skip the '*'
        self.position += 1;

        let mut nesting = 1;

        while nesting > 0 {
            if let Some(c) = self.input.next() {
                self.position += 1;

                if c == '/' && self.input.peek() == Some(&'*') {
                    self.input.next();
                    self.position += 1;
                    nesting += 1;
                } else if c == '*' && self.input.peek() == Some(&'/') {
                    self.input.next();
                    self.position += 1;
                    nesting -= 1;
                }
            } else {
                // Unexpected EOF inside comment
                break;
            }
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();

        while let Some(&c) = self.input.peek() {
            if c.is_alphanumeric() || c == '_' {
                identifier.push(c);
                self.input.next();
                self.position += 1;
            } else {
                break;
            }
        }

        identifier
    }

    fn read_number(&mut self) -> String {
        let mut number = String::new();
        let mut has_decimal = false;

        while let Some(&c) = self.input.peek() {
            if c.is_digit(10) {
                number.push(c);
                self.input.next();
                self.position += 1;
            } else if c == '.' && !has_decimal {
                has_decimal = true;
                number.push(c);
                self.input.next();
                self.position += 1;
            } else {
                break;
            }
        }

        number
    }

    fn read_string(&mut self) -> String {
        let mut string_content = String::new();

        while let Some(&c) = self.input.peek() {
            self.input.next();
            self.position += 1;

            if c == '"' {
                break;
            } else if c == '\\' {
                // Handle escape sequences
                if let Some(&next_char) = self.input.peek() {
                    string_content.push('\\');
                    string_content.push(next_char);
                    self.input.next();
                    self.position += 1;
                }
            } else {
                string_content.push(c);
            }
        }

        string_content
    }

    fn read_char(&mut self) -> String {
        let mut char_content = String::new();

        if let Some(&c) = self.input.peek() {
            if c == '\\' {
                // Handle escape sequences
                char_content.push(c);
                self.input.next();
                self.position += 1;

                if let Some(&next_char) = self.input.peek() {
                    char_content.push(next_char);
                    self.input.next();
                    self.position += 1;
                }
            } else {
                char_content.push(c);
                self.input.next();
                self.position += 1;
            }
        }

        // Skip the closing quote
        if let Some(&c) = self.input.peek() {
            if c == '\'' {
                self.input.next();
                self.position += 1;
            }
        }

        char_content
    }
}

// AST Node Types
#[derive(Debug, PartialEq)]
pub enum ASTNode {
    Program(Program),
    PackageDecl(String),
    ImportDecl(Vec<String>),
    FunctionDecl(Function),
    VarDecl(Vec<VarSpec>),
    ConstDecl(Vec<ConstSpec>),
    TypeDecl(Vec<TypeSpec>),
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub package: String,
    pub imports: Vec<String>,
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Function(Function),
    Variable(Vec<VarSpec>),
    Constant(Vec<ConstSpec>),
    Type(Vec<TypeSpec>),
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<String>,
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub param_type: String,
}

#[derive(Debug, PartialEq)]
pub struct VarSpec {
    names: Vec<String>,
    var_type: Option<String>,
    values: Option<Vec<Expression>>,
}

#[derive(Debug, PartialEq)]
pub struct ConstSpec {
    names: Vec<String>,
    const_type: Option<String>,
    values: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct TypeSpec {
    name: String,
    type_value: TypeValue,
}

#[derive(Debug, PartialEq)]
enum TypeValue {
    Basic(String),
    Struct(Vec<StructField>),
    Interface(Vec<InterfaceMethod>),
    Array(Box<TypeValue>, usize),
    Slice(Box<TypeValue>),
    Map(Box<TypeValue>, Box<TypeValue>),
    Pointer(Box<TypeValue>),
}

#[derive(Debug, PartialEq)]
struct StructField {
    names: Vec<String>,
    field_type: String,
    tag: Option<String>,
}

#[derive(Debug, PartialEq)]
struct InterfaceMethod {
    name: String,
    parameters: Vec<Parameter>,
    return_type: Option<String>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    ExpressionStmt(Expression),
    AssignmentStmt(Vec<Expression>, Vec<Expression>),
    ShortDeclStmt(Vec<String>, Vec<Expression>),
    ReturnStmt(Option<Expression>),
    IfStmt(Expression, Block, Option<Box<Statement>>),
    ForStmt(
        Option<Box<Statement>>,
        Option<Expression>,
        Option<Box<Statement>>,
        Block,
    ),
    Block(Block),
    // Add more statement types as needed
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    CharLiteral(char),
    BinaryExpr(Box<Expression>, BinaryOp, Box<Expression>),
    UnaryExpr(UnaryOp, Box<Expression>),
    CallExpr(Box<Expression>, Vec<Expression>),
    IndexExpr(Box<Expression>, Box<Expression>),
    FieldAccessExpr(Box<Expression>, String),
    // Add more expression types as needed
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or,
    // Add more binary operators as needed
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
    Pointer,
    Deref,
    // Add more unary operators as needed
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(source: &str) -> Self {
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token();
            let is_eof = token.token_type == TokenType::EOF;
            tokens.push(token);

            if is_eof {
                break;
            }
        }

        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Program, String> {
        let package = self.parse_package_decl()?;
        let imports = self.parse_imports()?;
        let declarations = self.parse_declarations()?;

        Ok(Program {
            package,
            imports,
            declarations,
        })
    }

    fn parse_package_decl(&mut self) -> Result<String, String> {
        self.consume(TokenType::Package, "Expected 'package' declaration")?;
        let package_name = self.consume_identifier("Expected package name")?;
        self.consume_optional_semicolon();

        Ok(package_name)
    }

    fn parse_imports(&mut self) -> Result<Vec<String>, String> {
        let mut imports = Vec::new();

        while self.check(TokenType::Import) {
            self.advance(); // Consume 'import'

            if self.check(TokenType::LParen) {
                self.advance(); // Consume '('

                while !self.check(TokenType::RParen) && !self.is_at_end() {
                    let import_path = self.parse_single_import()?;
                    imports.push(import_path);
                }

                self.consume(TokenType::RParen, "Expected ')' after import specs")?;
            } else {
                let import_path = self.parse_single_import()?;
                imports.push(import_path);
            }

            self.consume_optional_semicolon();
        }

        Ok(imports)
    }

    fn parse_single_import(&mut self) -> Result<String, String> {
        if self.check(TokenType::StringLiteral) {
            let import_path = self.current_token().lexeme.clone();
            self.advance(); // Consume string literal
            self.consume_optional_semicolon();
            Ok(import_path)
        } else {
            Err("Expected string literal for import path".to_string())
        }
    }

    fn parse_declarations(&mut self) -> Result<Vec<Declaration>, String> {
        let mut declarations = Vec::new();

        while !self.is_at_end() {
            if self.check(TokenType::Func) {
                declarations.push(Declaration::Function(self.parse_function_decl()?));
            } else if self.check(TokenType::Var) {
                declarations.push(Declaration::Variable(self.parse_var_decl()?));
            } else if self.check(TokenType::Const) {
                declarations.push(Declaration::Constant(self.parse_const_decl()?));
            } else if self.check(TokenType::Type) {
                declarations.push(Declaration::Type(self.parse_type_decl()?));
            } else {
                return Err(format!("Unexpected token: {:?}", self.current_token()));
            }
        }

        Ok(declarations)
    }

    fn parse_function_decl(&mut self) -> Result<Function, String> {
        self.consume(TokenType::Func, "Expected 'func' keyword")?;
        let name = self.consume_identifier("Expected function name")?;

        // Parse parameters
        self.consume(TokenType::LParen, "Expected '(' after function name")?;
        let parameters = self.parse_parameters()?;
        self.consume(TokenType::RParen, "Expected ')' after parameters")?;

        // Parse return type (optional)
        let return_type = if !self.check(TokenType::LBrace) {
            Some(self.parse_type()?)
        } else {
            None
        };

        // Parse function body
        let body = self.parse_block()?;
        self.consume_optional_semicolon();

        Ok(Function {
            name,
            parameters,
            return_type,
            body,
        })
    }

    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, String> {
        let mut parameters = Vec::new();

        if !self.check(TokenType::RParen) {
            loop {
                let name = self.consume_identifier("Expected parameter name")?;
                let param_type = self.parse_type()?;

                parameters.push(Parameter { name, param_type });

                if !self.check(TokenType::Comma) {
                    break;
                }

                self.advance(); // Consume comma
            }
        }

        Ok(parameters)
    }

    fn parse_type(&mut self) -> Result<String, String> {
        let type_name = self.consume_identifier("Expected type")?;
        Ok(type_name)
    }

    fn parse_var_decl(&mut self) -> Result<Vec<VarSpec>, String> {
        self.consume(TokenType::Var, "Expected 'var' keyword")?;
        let var_specs = self.parse_var_specs()?;
        self.consume_optional_semicolon();

        Ok(var_specs)
    }

    fn parse_var_specs(&mut self) -> Result<Vec<VarSpec>, String> {
        let mut var_specs = Vec::new();

        if self.check(TokenType::LParen) {
            self.advance(); // Consume '('

            while !self.check(TokenType::RParen) && !self.is_at_end() {
                var_specs.push(self.parse_var_spec()?);
                self.consume_optional_semicolon();
            }

            self.consume(TokenType::RParen, "Expected ')' after var specs")?;
        } else {
            var_specs.push(self.parse_var_spec()?);
        }

        Ok(var_specs)
    }

    fn parse_var_spec(&mut self) -> Result<VarSpec, String> {
        let names = self.parse_identifier_list()?;

        let var_type = if !self.check(TokenType::Assign) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let values = if self.check(TokenType::Assign) {
            self.advance(); // Consume '='
            Some(self.parse_expression_list()?)
        } else {
            None
        };

        Ok(VarSpec {
            names,
            var_type,
            values,
        })
    }

    fn parse_const_decl(&mut self) -> Result<Vec<ConstSpec>, String> {
        self.consume(TokenType::Const, "Expected 'const' keyword")?;
        let const_specs = self.parse_const_specs()?;
        self.consume_optional_semicolon();

        Ok(const_specs)
    }

    fn parse_const_specs(&mut self) -> Result<Vec<ConstSpec>, String> {
        let mut const_specs = Vec::new();

        if self.check(TokenType::LParen) {
            self.advance(); // Consume '('

            while !self.check(TokenType::RParen) && !self.is_at_end() {
                const_specs.push(self.parse_const_spec()?);
                self.consume_optional_semicolon();
            }

            self.consume(TokenType::RParen, "Expected ')' after const specs")?;
        } else {
            const_specs.push(self.parse_const_spec()?);
        }

        Ok(const_specs)
    }

    fn parse_const_spec(&mut self) -> Result<ConstSpec, String> {
        let names = self.parse_identifier_list()?;

        let const_type = if !self.check(TokenType::Assign) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.consume(TokenType::Assign, "Expected '=' after const names")?;
        let values = self.parse_expression_list()?;

        Ok(ConstSpec {
            names,
            const_type,
            values,
        })
    }

    fn parse_type_decl(&mut self) -> Result<Vec<TypeSpec>, String> {
        self.consume(TokenType::Type, "Expected 'type' keyword")?;
        let type_specs = self.parse_type_specs()?;
        self.consume_optional_semicolon();

        Ok(type_specs)
    }

    fn parse_type_specs(&mut self) -> Result<Vec<TypeSpec>, String> {
        let mut type_specs = Vec::new();

        if self.check(TokenType::LParen) {
            self.advance(); // Consume '('

            while !self.check(TokenType::RParen) && !self.is_at_end() {
                type_specs.push(self.parse_type_spec()?);
                self.consume_optional_semicolon();
            }

            self.consume(TokenType::RParen, "Expected ')' after type specs")?;
        } else {
            type_specs.push(self.parse_type_spec()?);
        }

        Ok(type_specs)
    }

    fn parse_type_spec(&mut self) -> Result<TypeSpec, String> {
        let name = self.consume_identifier("Expected type name")?;
        let type_value = self.parse_type_value()?;

        Ok(TypeSpec { name, type_value })
    }

    fn parse_type_value(&mut self) -> Result<TypeValue, String> {
        if self.check(TokenType::Struct) {
            self.advance(); // Consume 'struct'
            self.consume(TokenType::LBrace, "Expected '{' after 'struct'")?;

            let mut fields = Vec::new();

            while !self.check(TokenType::RBrace) && !self.is_at_end() {
                fields.push(self.parse_struct_field()?);
                self.consume_optional_semicolon();
            }

            self.consume(TokenType::RBrace, "Expected '}' after struct fields")?;

            Ok(TypeValue::Struct(fields))
        } else if self.check(TokenType::Interface) {
            self.advance(); // Consume 'interface'
            self.consume(TokenType::LBrace, "Expected '{' after 'interface'")?;

            let mut methods = Vec::new();

            while !self.check(TokenType::RBrace) && !self.is_at_end() {
                methods.push(self.parse_interface_method()?);
                self.consume_optional_semicolon();
            }

            self.consume(TokenType::RBrace, "Expected '}' after interface methods")?;

            Ok(TypeValue::Interface(methods))
        } else {
            // Basic type or composite type
            let type_name = self.parse_type()?;
            Ok(TypeValue::Basic(type_name))
        }
    }

    fn parse_struct_field(&mut self) -> Result<StructField, String> {
        let names = self.parse_identifier_list()?;
        let field_type = self.parse_type()?;

        let tag = if self.check(TokenType::StringLiteral) {
            let tag_value = self.current_token().lexeme.clone();
            self.advance(); // Consume string literal
            Some(tag_value)
        } else {
            None
        };

        Ok(StructField {
            names,
            field_type,
            tag,
        })
    }

    fn parse_interface_method(&mut self) -> Result<InterfaceMethod, String> {
        let name = self.consume_identifier("Expected method name")?;

        self.consume(TokenType::LParen, "Expected '(' after method name")?;
        let parameters = self.parse_parameters()?;
        self.consume(TokenType::RParen, "Expected ')' after parameters")?;

        let return_type = if !self.check(TokenType::Semicolon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        Ok(InterfaceMethod {
            name,
            parameters,
            return_type,
        })
    }

    fn parse_block(&mut self) -> Result<Block, String> {
        self.consume(TokenType::LBrace, "Expected '{'")?;

        let mut statements = Vec::new();

        while !self.check(TokenType::RBrace) && !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }

        self.consume(TokenType::RBrace, "Expected '}'")?;

        Ok(Block { statements })
    }

    fn parse_short_declaration(&mut self, left_expr: Expression) -> Result<Statement, String> {
        // Ensure left side is an identifier or comma-separated list of identifiers
        let mut identifiers = Vec::new();

        // Extract identifier from first expression
        if let Expression::Identifier(name) = left_expr {
            identifiers.push(name);
        } else {
            return Err("Left side of := must be an identifier".to_string());
        }

        // Handle multiple identifiers (e.g., x, y := 1, 2)
        while self.check(TokenType::Comma) {
            self.advance(); // Consume comma

            if self.check(TokenType::Identifier) {
                let name = self.current_token().lexeme.clone();
                self.advance(); // Consume identifier
                identifiers.push(name);
            } else {
                return Err("Expected identifier after comma in short declaration".to_string());
            }
        }

        // Consume := token
        self.consume(
            TokenType::ShortDeclare,
            "Expected ':=' in short declaration",
        )?;

        // Parse right side expressions
        let right_exprs = self.parse_expression_list()?;
        self.consume_optional_semicolon();

        Ok(Statement::ShortDeclStmt(identifiers, right_exprs))
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        if self.check(TokenType::Return) {
            self.parse_return_statement()
        } else if self.check(TokenType::If) {
            self.parse_if_statement()
        } else if self.check(TokenType::For) {
            self.parse_for_statement()
        } else if self.check(TokenType::LBrace) {
            Ok(Statement::Block(self.parse_block()?))
        } else {
            // First parse identifiers or expression
            let expr = self.parse_expression()?;

            if self.check(TokenType::ShortDeclare) {
                // Handle short variable declaration (:=)
                self.parse_short_declaration(expr)
            } else if self.check(TokenType::Assign) || self.check_compound_assignment() {
                // Handle regular assignment (=, +=, etc.)
                self.parse_assignment_statement(expr)
            } else {
                self.consume_optional_semicolon();
                Ok(Statement::ExpressionStmt(expr))
            }
        }
    }

    fn check_compound_assignment(&self) -> bool {
        matches!(
            self.current_token().token_type,
            TokenType::Plus
                | TokenType::Minus
                | TokenType::Asterisk
                | TokenType::Slash
                | TokenType::Percent
        ) && self.peek_token().token_type == TokenType::Assign
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.advance(); // Consume 'return'

        let expr = if !self.check(TokenType::Semicolon) && !self.check(TokenType::RBrace) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume_optional_semicolon();

        Ok(Statement::ReturnStmt(expr))
    }

    fn parse_if_statement(&mut self) -> Result<Statement, String> {
        self.advance(); // Consume 'if'

        // Optional initialization statement
        let init = if !self.check(TokenType::LBrace) && !self.check(TokenType::Semicolon) {
            let init_expr = self.parse_expression()?;

            if self.check(TokenType::Semicolon) {
                self.advance(); // Consume semicolon
                Some(Box::new(Statement::ExpressionStmt(init_expr)))
            } else {
                None
            }
        } else {
            None
        };

        // Condition expression
        let condition = if init.is_some() {
            self.parse_expression()?
        } else {
            self.parse_expression()?
        };

        // If block
        let if_block = self.parse_block()?;

        // Optional else
        let else_branch = if self.check(TokenType::Else) {
            self.advance(); // Consume 'else'

            if self.check(TokenType::If) {
                Some(Box::new(self.parse_if_statement()?))
            } else {
                Some(Box::new(Statement::Block(self.parse_block()?)))
            }
        } else {
            None
        };

        self.consume_optional_semicolon();

        Ok(Statement::IfStmt(condition, if_block, else_branch))
    }

    fn parse_for_statement(&mut self) -> Result<Statement, String> {
        self.advance(); // Consume 'for'

        if self.check(TokenType::LBrace) {
            // Infinite loop
            let block = self.parse_block()?;
            return Ok(Statement::ForStmt(None, None, None, block));
        }

        // Try to parse a condition
        let init_cond_post = self.parse_expression()?;

        if self.check(TokenType::LBrace) {
            // Condition-only loop
            let block = self.parse_block()?;
            return Ok(Statement::ForStmt(None, Some(init_cond_post), None, block));
        }

        self.consume(TokenType::Semicolon, "Expected ';' in for clause")?;

        // Parse the condition
        let condition = if !self.check(TokenType::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume(TokenType::Semicolon, "Expected ';' in for clause")?;

        // Parse the post statement
        let post = if !self.check(TokenType::LBrace) {
            Some(Box::new(Statement::ExpressionStmt(
                self.parse_expression()?,
            )))
        } else {
            None
        };

        let block = self.parse_block()?;

        Ok(Statement::ForStmt(
            Some(Box::new(Statement::ExpressionStmt(init_cond_post))),
            condition,
            post.map(|p| *Box::new(p)),
            block,
        ))
    }

    fn parse_assignment_statement(&mut self, left_expr: Expression) -> Result<Statement, String> {
        let mut left_exprs = vec![left_expr];

        // Handle multiple assignments (e.g., a, b = 1, 2)
        while self.check(TokenType::Comma) {
            self.advance(); // Consume comma
            left_exprs.push(self.parse_expression()?);
        }

        // Handle the assignment operator
        self.advance(); // Consume assignment operator

        let right_exprs = self.parse_expression_list()?;
        self.consume_optional_semicolon();

        Ok(Statement::AssignmentStmt(left_exprs, right_exprs))
    }

    fn parse_expression_list(&mut self) -> Result<Vec<Expression>, String> {
        let mut expressions = vec![self.parse_expression()?];

        while self.check(TokenType::Comma) {
            self.advance(); // Consume comma
            expressions.push(self.parse_expression()?);
        }

        Ok(expressions)
    }

    fn parse_expression(&mut self) -> Result<Expression, String> {
        self.parse_logical_or()
    }

    fn parse_logical_or(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_logical_and()?;

        while self.check(TokenType::Or) {
            let operator = BinaryOp::Or;
            self.advance(); // Consume operator
            let right = self.parse_logical_and()?;
            expr = Expression::BinaryExpr(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_equality()?;

        while self.check(TokenType::And) {
            let operator = BinaryOp::And;
            self.advance(); // Consume operator
            let right = self.parse_equality()?;
            expr = Expression::BinaryExpr(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_comparison()?;

        while self.check(TokenType::Equal) || self.check(TokenType::NotEqual) {
            let operator = match self.current_token().token_type {
                TokenType::Equal => BinaryOp::Eq,
                TokenType::NotEqual => BinaryOp::NotEq,
                _ => unreachable!(),
            };

            self.advance(); // Consume operator
            let right = self.parse_comparison()?;
            expr = Expression::BinaryExpr(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_term()?;

        while self.check(TokenType::LessThan)
            || self.check(TokenType::GreaterThan)
            || self.check(TokenType::LessEqual)
            || self.check(TokenType::GreaterEqual)
        {
            let operator = match self.current_token().token_type {
                TokenType::LessThan => BinaryOp::Lt,
                TokenType::GreaterThan => BinaryOp::Gt,
                TokenType::LessEqual => BinaryOp::LtEq,
                TokenType::GreaterEqual => BinaryOp::GtEq,
                _ => unreachable!(),
            };

            self.advance(); // Consume operator
            let right = self.parse_term()?;
            expr = Expression::BinaryExpr(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_factor()?;

        while self.check(TokenType::Plus) || self.check(TokenType::Minus) {
            let operator = match self.current_token().token_type {
                TokenType::Plus => BinaryOp::Add,
                TokenType::Minus => BinaryOp::Sub,
                _ => unreachable!(),
            };

            self.advance(); // Consume operator
            let right = self.parse_factor()?;
            expr = Expression::BinaryExpr(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_unary()?;

        while self.check(TokenType::Asterisk)
            || self.check(TokenType::Slash)
            || self.check(TokenType::Percent)
        {
            let operator = match self.current_token().token_type {
                TokenType::Asterisk => BinaryOp::Mul,
                TokenType::Slash => BinaryOp::Div,
                TokenType::Percent => BinaryOp::Mod,
                _ => unreachable!(),
            };

            self.advance(); // Consume operator
            let right = self.parse_unary()?;
            expr = Expression::BinaryExpr(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expression, String> {
        if self.check(TokenType::Minus) || self.check(TokenType::Not) {
            let operator = match self.current_token().token_type {
                TokenType::Minus => UnaryOp::Neg,
                TokenType::Not => UnaryOp::Not,
                _ => unreachable!(),
            };

            self.advance(); // Consume operator
            let right = self.parse_unary()?;
            Ok(Expression::UnaryExpr(operator, Box::new(right)))
        } else {
            self.parse_primary()
        }
    }

    fn parse_primary(&mut self) -> Result<Expression, String> {
        let expr = if self.check(TokenType::Identifier) {
            let identifier = self.current_token().lexeme.clone();
            self.advance(); // Consume identifier
            Expression::Identifier(identifier)
        } else if self.check(TokenType::IntLiteral) {
            let value = self
                .current_token()
                .lexeme
                .parse::<i64>()
                .map_err(|_| "Failed to parse integer literal".to_string())?;

            self.advance(); // Consume literal
            Expression::IntLiteral(value)
        } else if self.check(TokenType::FloatLiteral) {
            let value = self
                .current_token()
                .lexeme
                .parse::<f64>()
                .map_err(|_| "Failed to parse float literal".to_string())?;

            self.advance(); // Consume literal
            Expression::FloatLiteral(value)
        } else if self.check(TokenType::StringLiteral) {
            let value = self.current_token().lexeme.clone();
            self.advance(); // Consume literal
            Expression::StringLiteral(value)
        } else if self.check(TokenType::CharLiteral) {
            let value = self
                .current_token()
                .lexeme
                .chars()
                .next()
                .ok_or_else(|| "Invalid char literal".to_string())?;

            self.advance(); // Consume literal
            Expression::CharLiteral(value)
        } else if self.check(TokenType::LParen) {
            self.advance(); // Consume '('
            let expr = self.parse_expression()?;
            self.consume(TokenType::RParen, "Expected ')'")?;
            expr
        } else {
            return Err(format!("Unexpected token: {:?}", self.current_token()));
        };

        self.parse_primary_suffix(expr)
    }

    fn parse_primary_suffix(&mut self, expr: Expression) -> Result<Expression, String> {
        let mut result = expr;

        loop {
            if self.check(TokenType::LParen) {
                // Function call
                self.advance(); // Consume '('
                let arguments = if !self.check(TokenType::RParen) {
                    self.parse_expression_list()?
                } else {
                    Vec::new()
                };

                self.consume(TokenType::RParen, "Expected ')' after arguments")?;
                result = Expression::CallExpr(Box::new(result), arguments);
            } else if self.check(TokenType::LBracket) {
                // Index expression
                self.advance(); // Consume '['
                let index = self.parse_expression()?;
                self.consume(TokenType::RBracket, "Expected ']' after index")?;
                result = Expression::IndexExpr(Box::new(result), Box::new(index));
            } else if self.check(TokenType::Period) {
                // Field access
                self.advance(); // Consume '.'
                let field = self.consume_identifier("Expected field name after '.'")?;
                result = Expression::FieldAccessExpr(Box::new(result), field);
            } else {
                break;
            }
        }

        Ok(result)
    }

    fn parse_identifier_list(&mut self) -> Result<Vec<String>, String> {
        let mut identifiers = vec![self.consume_identifier("Expected identifier")?];

        while self.check(TokenType::Comma) {
            self.advance(); // Consume comma
            identifiers.push(self.consume_identifier("Expected identifier after comma")?);
        }

        Ok(identifiers)
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<(), String> {
        if self.check(token_type) {
            self.advance();
            Ok(())
        } else {
            Err(format!(
                "{} at position {}, found {:?}",
                message,
                self.current_token().position,
                self.current_token().token_type
            ))
        }
    }

    fn consume_identifier(&mut self, message: &str) -> Result<String, String> {
        if self.check(TokenType::Identifier) {
            let identifier = self.current_token().lexeme.clone();
            self.advance();
            Ok(identifier)
        } else {
            Err(format!(
                "{} at position {}, found {:?}",
                message,
                self.current_token().position,
                self.current_token().token_type
            ))
        }
    }

    fn consume_optional_semicolon(&mut self) {
        if self.check(TokenType::Semicolon) {
            self.advance();
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.current_token().token_type == token_type
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous_token()
    }

    fn is_at_end(&self) -> bool {
        self.current_token().token_type == TokenType::EOF
    }

    fn current_token(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous_token(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn peek_token(&self) -> &Token {
        if self.current + 1 >= self.tokens.len() {
            &self.tokens[self.current]
        } else {
            &self.tokens[self.current + 1]
        }
    }
}
