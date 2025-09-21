// Copyright 2025-present Raphael Amorim. All rights reserved.
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
    Increment,
    Decrement,
    PlusAssign,
    MinusAssign,

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

                    Token {
                        token_type,
                        lexeme: identifier,
                        position: start_pos,
                    }
                }
                '0'..='9' => {
                    let number = self.read_number();
                    let token_type = if number.contains('.') {
                        TokenType::FloatLiteral
                    } else {
                        TokenType::IntLiteral
                    };

                    Token {
                        token_type,
                        lexeme: number,
                        position: start_pos,
                    }
                }
                '.' => {
                    // Check if this starts a float literal (e.g., .5)
                    let mut temp_chars = self.input.clone();
                    temp_chars.next(); // Skip the '.'

                    if let Some(&next_char) = temp_chars.peek() {
                        if next_char.is_ascii_digit() {
                            // This is a float literal starting with '.'
                            let number = self.read_number();
                            return Token {
                                token_type: TokenType::FloatLiteral,
                                lexeme: number,
                                position: start_pos,
                            };
                        }
                    }

                    // This is just a period token
                    self.create_simple_token(TokenType::Period, ".")
                }
                '"' => {
                    self.input.next();
                    self.position += 1;
                    let string_content = self.read_string();

                    Token {
                        token_type: TokenType::StringLiteral,
                        lexeme: string_content,
                        position: start_pos,
                    }
                }
                '\'' => {
                    self.input.next();
                    self.position += 1;
                    let char_content = self.read_char();

                    Token {
                        token_type: TokenType::CharLiteral,
                        lexeme: char_content,
                        position: start_pos,
                    }
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

                    Token {
                        token_type: TokenType::Slash,
                        lexeme: "/".to_string(),
                        position: start_pos,
                    }
                }
                '+' => {
                    self.input.next();
                    self.position += 1;

                    if let Some(&next_char) = self.input.peek() {
                        if next_char == '+' {
                            self.input.next();
                            self.position += 1;
                            return Token {
                                token_type: TokenType::Increment,
                                lexeme: "++".to_string(),
                                position: start_pos,
                            };
                        } else if next_char == '=' {
                            self.input.next();
                            self.position += 1;
                            return Token {
                                token_type: TokenType::PlusAssign,
                                lexeme: "+=".to_string(),
                                position: start_pos,
                            };
                        }
                    }

                    return Token {
                        token_type: TokenType::Plus,
                        lexeme: "+".to_string(),
                        position: start_pos,
                    };
                }
                '-' => {
                    self.input.next();
                    self.position += 1;

                    if let Some(&next_char) = self.input.peek() {
                        if next_char == '-' {
                            self.input.next();
                            self.position += 1;
                            return Token {
                                token_type: TokenType::Decrement,
                                lexeme: "--".to_string(),
                                position: start_pos,
                            };
                        } else if next_char == '=' {
                            self.input.next();
                            self.position += 1;
                            return Token {
                                token_type: TokenType::MinusAssign,
                                lexeme: "-=".to_string(),
                                position: start_pos,
                            };
                        }
                    }

                    return Token {
                        token_type: TokenType::Minus,
                        lexeme: "-".to_string(),
                        position: start_pos,
                    };
                }
                '*' => self.create_simple_token(TokenType::Asterisk, "*"),
                '%' => self.create_simple_token(TokenType::Percent, "%"),
                '(' => self.create_simple_token(TokenType::LParen, "("),
                ')' => self.create_simple_token(TokenType::RParen, ")"),
                '{' => self.create_simple_token(TokenType::LBrace, "{"),
                '}' => self.create_simple_token(TokenType::RBrace, "}"),
                '[' => self.create_simple_token(TokenType::LBracket, "["),
                ']' => self.create_simple_token(TokenType::RBracket, "]"),
                ',' => self.create_simple_token(TokenType::Comma, ","),
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

                    Token {
                        token_type: TokenType::Colon,
                        lexeme: ":".to_string(),
                        position: start_pos,
                    }
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

                    Token {
                        token_type: TokenType::Assign,
                        lexeme: "=".to_string(),
                        position: start_pos,
                    }
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

                    Token {
                        token_type: TokenType::Not,
                        lexeme: "!".to_string(),
                        position: start_pos,
                    }
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

                    Token {
                        token_type: TokenType::LessThan,
                        lexeme: "<".to_string(),
                        position: start_pos,
                    }
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

                    Token {
                        token_type: TokenType::GreaterThan,
                        lexeme: ">".to_string(),
                        position: start_pos,
                    }
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
                    Token {
                        token_type: TokenType::And,
                        lexeme: "&".to_string(),
                        position: start_pos,
                    }
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
                    Token {
                        token_type: TokenType::Or,
                        lexeme: "|".to_string(),
                        position: start_pos,
                    }
                }
                _ => {
                    // Check if this is a printable ASCII character we don't recognize
                    if c.is_ascii_graphic() {
                        // For debugging: let's see what character is causing issues
                        eprintln!(
                            "Warning: Skipping unrecognized character '{}' (0x{:02x}) at position {}",
                            c, c as u8, self.position
                        );
                    }

                    self.input.next();
                    self.position += 1;
                    return self.next_token(); // Skip and try again
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

        // Handle leading decimal point (e.g., .5)
        if let Some(&'.') = self.input.peek() {
            number.push('.');
            self.input.next();
            self.position += 1;
            has_decimal = true;
        }

        while let Some(&c) = self.input.peek() {
            if c.is_ascii_digit() {
                number.push(c);
                self.input.next();
                self.position += 1;
            } else if c == '.' && !has_decimal {
                // Check if the next character after the dot is a digit
                // We need to peek ahead two characters to make this decision
                let mut temp_chars = self.input.clone();
                temp_chars.next(); // Skip the '.'

                if let Some(&next_char) = temp_chars.peek() {
                    if next_char.is_ascii_digit() {
                        has_decimal = true;
                        number.push(c);
                        self.input.next();
                        self.position += 1;
                    } else {
                        // The '.' is not part of this number (likely field access)
                        break;
                    }
                } else {
                    // End of input after '.', not part of number
                    break;
                }
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
                    self.input.next();
                    self.position += 1;

                    match next_char {
                        '"' => string_content.push('"'),
                        '\\' => string_content.push('\\'),
                        'n' => string_content.push('\n'),
                        't' => string_content.push('\t'),
                        'r' => string_content.push('\r'),
                        _ => {
                            // For unrecognized escape sequences, keep the backslash
                            string_content.push('\\');
                            string_content.push(next_char);
                        }
                    }
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
    pub names: Vec<String>,
    pub var_type: Option<String>,
    pub values: Option<Vec<Expression>>,
}

#[derive(Debug, PartialEq)]
pub struct ConstSpec {
    pub names: Vec<String>,
    pub const_type: Option<String>,
    pub values: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct TypeSpec {
    pub name: String,
    pub type_value: TypeValue,
}

#[derive(Debug, PartialEq)]
pub enum TypeValue {
    Basic(String),
    Struct(Vec<StructField>),
    Interface(Vec<InterfaceMethod>),
    Array(Box<TypeValue>, usize),
    Slice(Box<TypeValue>),
    Map(Box<TypeValue>, Box<TypeValue>),
    Pointer(Box<TypeValue>),
}

#[derive(Debug, PartialEq)]
pub struct StructField {
    pub names: Vec<String>,
    pub field_type: String,
    pub tag: Option<String>,
}

#[derive(Debug, PartialEq)]
pub struct InterfaceMethod {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<String>,
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
    IncrementStmt(Expression),
    DecrementStmt(Expression),
    VarDeclStmt(Vec<VarSpec>),
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
    StructLiteral(String, Vec<FieldValue>),
}

#[derive(Debug, PartialEq)]
pub struct FieldValue {
    pub name: String,
    pub value: Expression,
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
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
    Pointer,
    Deref,
}

#[derive(Debug)]
enum StatementOrExpression {
    Statement(Statement),
    Expression(Expression),
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

    fn parse_var_decl_stmt(&mut self) -> Result<Vec<VarSpec>, String> {
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

        let return_type = if !self.check(TokenType::Semicolon)
            && !self.check(TokenType::RBrace)
            && !self.is_at_end()
        {
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
        let mut identifiers = Vec::new();
        let mut left_exprs = vec![left_expr];

        // Handle multiple identifiers on the left (e.g., x, y := 1, 2)
        while self.check(TokenType::Comma) {
            self.advance(); // Consume comma
            left_exprs.push(self.parse_expression()?);
        }

        // Extract identifiers from expressions
        for expr in left_exprs {
            if let Expression::Identifier(name) = expr {
                identifiers.push(name);
            } else {
                return Err("Left side of := must be identifiers".to_string());
            }
        }

        // Consume := token
        self.consume(
            TokenType::ShortDeclare,
            "Expected ':=' in short declaration",
        )?;

        // Parse right side expressions
        let right_exprs = self.parse_expression_list()?;

        Ok(Statement::ShortDeclStmt(identifiers, right_exprs))
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        if self.check(TokenType::Return) {
            self.parse_return_statement()
        } else if self.check(TokenType::If) {
            self.parse_if_statement()
        } else if self.check(TokenType::For) {
            self.parse_for_statement()
        } else if self.check(TokenType::Var) {
            // Handle variable declaration statement
            self.advance(); // consume 'var'
            let var_specs = self.parse_var_decl_stmt()?;
            Ok(Statement::VarDeclStmt(var_specs))
        } else if self.check(TokenType::LBrace) {
            Ok(Statement::Block(self.parse_block()?))
        } else {
            // First parse expression
            let expr = self.parse_expression()?;

            if self.check(TokenType::ShortDeclare) {
                // Handle short variable declaration (:=)
                self.parse_short_declaration(expr)
            } else if self.check(TokenType::Assign) || self.check_compound_assignment() {
                // Handle regular assignment (=, +=, etc.)
                self.parse_assignment_statement(expr)
            } else if self.check(TokenType::Increment) {
                // Handle increment statement (e.g., i++)
                self.advance(); // consume ++
                self.consume_optional_semicolon();
                Ok(Statement::IncrementStmt(expr))
            } else if self.check(TokenType::Decrement) {
                // Handle decrement statement (e.g., i--)
                self.advance(); // consume --
                self.consume_optional_semicolon();
                Ok(Statement::DecrementStmt(expr))
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
                | TokenType::PlusAssign
                | TokenType::MinusAssign
        ) && (self.peek_token().token_type == TokenType::Assign
            || matches!(
                self.current_token().token_type,
                TokenType::PlusAssign | TokenType::MinusAssign
            ))
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

        let condition = self.parse_expression()?;
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

        // Parse first part (could be init statement or condition)
        let first_part = self.parse_statement_or_expression()?;

        if self.check(TokenType::LBrace) {
            // Condition-only loop (e.g., for x > 0 { ... })
            if let StatementOrExpression::Expression(expr) = first_part {
                let block = self.parse_block()?;
                return Ok(Statement::ForStmt(None, Some(expr), None, block));
            } else {
                return Err("Expected expression for condition-only for loop".to_string());
            }
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
            Some(Box::new(self.parse_post_statement()?))
        } else {
            None
        };

        let block = self.parse_block()?;

        let init = match first_part {
            StatementOrExpression::Statement(stmt) => Some(Box::new(stmt)),
            StatementOrExpression::Expression(expr) => {
                Some(Box::new(Statement::ExpressionStmt(expr)))
            }
        };

        Ok(Statement::ForStmt(init, condition, post, block))
    }

    fn parse_statement_or_expression(&mut self) -> Result<StatementOrExpression, String> {
        // Parse an expression first
        let expr = self.parse_expression()?;

        // Check if it's followed by assignment operators
        if self.check(TokenType::ShortDeclare) {
            Ok(StatementOrExpression::Statement(
                self.parse_short_declaration(expr)?,
            ))
        } else if self.check(TokenType::Assign) || self.check_compound_assignment() {
            Ok(StatementOrExpression::Statement(
                self.parse_assignment_statement(expr)?,
            ))
        } else {
            Ok(StatementOrExpression::Expression(expr))
        }
    }

    fn parse_post_statement(&mut self) -> Result<Statement, String> {
        let expr = self.parse_expression()?;

        if self.check(TokenType::Increment) {
            self.advance(); // consume ++
            Ok(Statement::IncrementStmt(expr))
        } else if self.check(TokenType::Decrement) {
            self.advance(); // consume --
            Ok(Statement::DecrementStmt(expr))
        } else if self.check(TokenType::Assign) || self.check_compound_assignment() {
            self.parse_assignment_statement(expr)
        } else {
            Ok(Statement::ExpressionStmt(expr))
        }
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

            // Check for struct literal: TypeName{field: value}
            if self.check(TokenType::LBrace) {
                self.advance(); // Consume '{'
                let fields = self.parse_struct_literal_fields()?;
                self.consume(TokenType::RBrace, "Expected '}' after struct fields")?;
                Expression::StructLiteral(identifier, fields)
            } else {
                Expression::Identifier(identifier)
            }
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

    fn parse_struct_literal_fields(&mut self) -> Result<Vec<FieldValue>, String> {
        let mut fields = Vec::new();

        if !self.check(TokenType::RBrace) {
            loop {
                let field_name = self.consume_identifier("Expected field name")?;
                self.consume(TokenType::Colon, "Expected ':' after field name")?;
                let field_value = self.parse_expression()?;

                fields.push(FieldValue {
                    name: field_name,
                    value: field_value,
                });

                if !self.check(TokenType::Comma) {
                    break;
                }

                self.advance(); // Consume comma
            }
        }

        Ok(fields)
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
                // CRITICAL FIX: Check bounds and token type before processing
                if self.current + 1 < self.tokens.len() {
                    let next_token = &self.tokens[self.current + 1];
                    if next_token.token_type == TokenType::Identifier {
                        self.advance(); // Consume '.'
                        let field = self.consume_identifier("Expected field name after '.'")?;
                        result = Expression::FieldAccessExpr(Box::new(result), field);
                    } else {
                        // Period is not followed by identifier - this is likely a spurious token
                        // Log it for debugging and skip it
                        eprintln!(
                            "Warning: Skipping spurious period token at position {} (followed by {:?})",
                            self.current_token().position,
                            next_token.token_type
                        );
                        self.advance(); // Skip the spurious period
                        // Don't try to parse it as field access, just continue
                    }
                } else {
                    // Period at end of token stream - skip it
                    eprintln!(
                        "Warning: Period token at end of token stream at position {}",
                        self.current_token().position
                    );
                    self.advance();
                }
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

    // And let's make the error messages more informative
    fn consume_identifier(&mut self, message: &str) -> Result<String, String> {
        if self.check(TokenType::Identifier) {
            let identifier = self.current_token().lexeme.clone();
            self.advance();
            Ok(identifier)
        } else {
            // Provide detailed error information for debugging
            let current_token = self.current_token();
            Err(format!(
                "{} at position {}, found {:?} ('{}') - current token index: {}",
                message,
                current_token.position,
                current_token.token_type,
                current_token.lexeme,
                self.current
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
            // Return the last token (should be EOF) if we're at the end
            &self.tokens[self.tokens.len() - 1]
        } else {
            &self.tokens[self.current + 1]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_increment_decrement() {
        let mut lexer = Lexer::new("++ -- += -=");

        assert_eq!(lexer.next_token().token_type, TokenType::Increment);
        assert_eq!(lexer.next_token().token_type, TokenType::Decrement);
        assert_eq!(lexer.next_token().token_type, TokenType::PlusAssign);
        assert_eq!(lexer.next_token().token_type, TokenType::MinusAssign);
    }

    #[test]
    fn test_parse_increment_decrement() {
        let mut parser = Parser::new(
            r#"
            package main
            func main() {
                i++
                j--
            }
        "#,
        );
        let program = parser.parse().unwrap();

        if let Declaration::Function(func) = &program.declarations[0] {
            if let Statement::IncrementStmt(expr) = &func.body.statements[0] {
                if let Expression::Identifier(name) = expr {
                    assert_eq!(name, "i");
                } else {
                    panic!("Expected identifier i");
                }
            } else {
                panic!("Expected increment statement");
            }

            if let Statement::DecrementStmt(expr) = &func.body.statements[1] {
                if let Expression::Identifier(name) = expr {
                    assert_eq!(name, "j");
                } else {
                    panic!("Expected identifier j");
                }
            } else {
                panic!("Expected decrement statement");
            }
        }
    }

    #[test]
    fn test_parse_for_loop_with_increment() {
        let mut parser = Parser::new(
            r#"
            package main
            func main() {
                for i := 0; i < 10; i++ {
                    return
                }
            }
        "#,
        );
        let program = parser.parse().unwrap();

        if let Declaration::Function(func) = &program.declarations[0] {
            if let Statement::ForStmt(init, condition, post, _block) = &func.body.statements[0] {
                assert!(init.is_some());
                assert!(condition.is_some());

                if let Some(post_stmt) = post {
                    if let Statement::IncrementStmt(expr) = &**post_stmt {
                        if let Expression::Identifier(name) = expr {
                            assert_eq!(name, "i");
                        } else {
                            panic!("Expected identifier i in increment");
                        }
                    } else {
                        panic!("Expected increment statement in post");
                    }
                } else {
                    panic!("Expected post statement");
                }
            } else {
                panic!("Expected for statement");
            }
        }
    }

    #[test]
    fn test_lexer_keywords() {
        let mut lexer = Lexer::new("package main func if else");

        assert_eq!(lexer.next_token().token_type, TokenType::Package);
        assert_eq!(lexer.next_token().token_type, TokenType::Identifier);
        assert_eq!(lexer.next_token().token_type, TokenType::Func);
        assert_eq!(lexer.next_token().token_type, TokenType::If);
        assert_eq!(lexer.next_token().token_type, TokenType::Else);
    }

    #[test]
    fn test_lexer_operators() {
        let mut lexer = Lexer::new("+ - * / % == != < > <= >= && || ! = :=");

        assert_eq!(lexer.next_token().token_type, TokenType::Plus);
        assert_eq!(lexer.next_token().token_type, TokenType::Minus);
        assert_eq!(lexer.next_token().token_type, TokenType::Asterisk);
        assert_eq!(lexer.next_token().token_type, TokenType::Slash);
        assert_eq!(lexer.next_token().token_type, TokenType::Percent);
        assert_eq!(lexer.next_token().token_type, TokenType::Equal);
        assert_eq!(lexer.next_token().token_type, TokenType::NotEqual);
        assert_eq!(lexer.next_token().token_type, TokenType::LessThan);
        assert_eq!(lexer.next_token().token_type, TokenType::GreaterThan);
        assert_eq!(lexer.next_token().token_type, TokenType::LessEqual);
        assert_eq!(lexer.next_token().token_type, TokenType::GreaterEqual);
        assert_eq!(lexer.next_token().token_type, TokenType::And);
        assert_eq!(lexer.next_token().token_type, TokenType::Or);
        assert_eq!(lexer.next_token().token_type, TokenType::Not);
        assert_eq!(lexer.next_token().token_type, TokenType::Assign);
        assert_eq!(lexer.next_token().token_type, TokenType::ShortDeclare);
    }

    #[test]
    fn test_lexer_literals() {
        let mut lexer = Lexer::new(r#"123 3.14 "hello" 'a'"#);

        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::IntLiteral);
        assert_eq!(token.lexeme, "123");

        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::FloatLiteral);
        assert_eq!(token.lexeme, "3.14");

        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::StringLiteral);
        assert_eq!(token.lexeme, "hello");

        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::CharLiteral);
        assert_eq!(token.lexeme, "a");
    }

    #[test]
    fn test_lexer_comments() {
        let mut lexer = Lexer::new("// This is a comment\npackage main");

        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::Package);
    }

    #[test]
    fn test_lexer_block_comments() {
        let mut lexer = Lexer::new("/* This is a\n   block comment */\nfunc main");

        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::Func);
    }

    #[test]
    fn test_parse_simple_package() {
        let mut parser = Parser::new("package main");
        let program = parser.parse().unwrap();

        assert_eq!(program.package, "main");
        assert!(program.imports.is_empty());
        assert!(program.declarations.is_empty());
    }

    #[test]
    fn test_parse_package_with_imports() {
        let mut parser = Parser::new(
            r#"
            package main
            import "fmt"
            import "os"
        "#,
        );
        let program = parser.parse().unwrap();

        assert_eq!(program.package, "main");
        assert_eq!(program.imports.len(), 2);
        assert_eq!(program.imports[0], "fmt");
        assert_eq!(program.imports[1], "os");
    }

    #[test]
    fn test_parse_grouped_imports() {
        let mut parser = Parser::new(
            r#"
            package main
            import (
                "fmt"
                "os"
            )
        "#,
        );
        let program = parser.parse().unwrap();

        assert_eq!(program.package, "main");
        assert_eq!(program.imports.len(), 2);
        assert_eq!(program.imports[0], "fmt");
        assert_eq!(program.imports[1], "os");
    }

    #[test]
    fn test_parse_simple_function() {
        let mut parser = Parser::new(
            r#"
            package main
            func main() {
                return
            }
        "#,
        );
        let program = parser.parse().unwrap();

        assert_eq!(program.declarations.len(), 1);
        if let Declaration::Function(func) = &program.declarations[0] {
            assert_eq!(func.name, "main");
            assert!(func.parameters.is_empty());
            assert!(func.return_type.is_none());
            assert_eq!(func.body.statements.len(), 1);

            if let Statement::ReturnStmt(None) = &func.body.statements[0] {
                // Correct
            } else {
                panic!("Expected return statement");
            }
        } else {
            panic!("Expected function declaration");
        }
    }

    #[test]
    fn test_parse_function_with_parameters() {
        let mut parser = Parser::new(
            r#"
            package main
            func add(x int, y int) int {
                return x + y
            }
        "#,
        );
        let program = parser.parse().unwrap();

        assert_eq!(program.declarations.len(), 1);
        if let Declaration::Function(func) = &program.declarations[0] {
            assert_eq!(func.name, "add");
            assert_eq!(func.parameters.len(), 2);
            assert_eq!(func.parameters[0].name, "x");
            assert_eq!(func.parameters[0].param_type, "int");
            assert_eq!(func.parameters[1].name, "y");
            assert_eq!(func.parameters[1].param_type, "int");
            assert_eq!(func.return_type, Some("int".to_string()));
        } else {
            panic!("Expected function declaration");
        }
    }

    #[test]
    fn test_parse_variable_declaration() {
        let mut parser = Parser::new(
            r#"
            package main
            var x int = 42
        "#,
        );
        let program = parser.parse().unwrap();

        assert_eq!(program.declarations.len(), 1);
        if let Declaration::Variable(var_specs) = &program.declarations[0] {
            assert_eq!(var_specs.len(), 1);
            assert_eq!(var_specs[0].names, vec!["x"]);
            assert_eq!(var_specs[0].var_type, Some("int".to_string()));

            if let Some(values) = &var_specs[0].values {
                assert_eq!(values.len(), 1);
                if let Expression::IntLiteral(42) = &values[0] {
                    // Correct
                } else {
                    panic!("Expected int literal 42");
                }
            } else {
                panic!("Expected variable value");
            }
        } else {
            panic!("Expected variable declaration");
        }
    }

    #[test]
    fn test_parse_short_declaration() {
        let mut parser = Parser::new(
            r#"
            package main
            func main() {
                x := 42
            }
        "#,
        );
        let program = parser.parse().unwrap();

        if let Declaration::Function(func) = &program.declarations[0] {
            if let Statement::ShortDeclStmt(names, values) = &func.body.statements[0] {
                assert_eq!(names, &vec!["x"]);
                assert_eq!(values.len(), 1);
                if let Expression::IntLiteral(42) = &values[0] {
                    // Correct
                } else {
                    panic!("Expected int literal 42");
                }
            } else {
                panic!("Expected short declaration statement");
            }
        }
    }

    #[test]
    fn test_parse_if_statement() {
        let mut parser = Parser::new(
            r#"
            package main
            func main() {
                if x > 0 {
                    return x
                }
            }
        "#,
        );
        let program = parser.parse().unwrap();

        if let Declaration::Function(func) = &program.declarations[0] {
            if let Statement::IfStmt(condition, _if_block, _else_branch) = &func.body.statements[0]
            {
                if let Expression::BinaryExpr(_, BinaryOp::Gt, _) = condition {
                    // Correct
                } else {
                    panic!("Expected binary expression with > operator");
                }
            } else {
                panic!("Expected if statement");
            }
        }
    }

    #[test]
    fn test_parse_for_loop() {
        let mut parser = Parser::new(
            r#"
            package main
            func main() {
                for i := 0; i < 10; i = i + 1 {
                    return
                }
            }
        "#,
        );
        let program = parser.parse().unwrap();

        if let Declaration::Function(func) = &program.declarations[0] {
            if let Statement::ForStmt(init, condition, post, _block) = &func.body.statements[0] {
                assert!(init.is_some());
                assert!(condition.is_some());
                assert!(post.is_some());
            } else {
                panic!("Expected for statement");
            }
        }
    }

    #[test]
    fn test_parse_struct_type() {
        let mut parser = Parser::new(
            r#"
            package main
            type Person struct {
                name string
                age int
            }
        "#,
        );
        let program = parser.parse().unwrap();

        if let Declaration::Type(type_specs) = &program.declarations[0] {
            assert_eq!(type_specs.len(), 1);
            assert_eq!(type_specs[0].name, "Person");

            if let TypeValue::Struct(fields) = &type_specs[0].type_value {
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].names, vec!["name"]);
                assert_eq!(fields[0].field_type, "string");
                assert_eq!(fields[1].names, vec!["age"]);
                assert_eq!(fields[1].field_type, "int");
            } else {
                panic!("Expected struct type");
            }
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn test_parse_struct_with_multiple_field_names() {
        let mut parser = Parser::new(
            r#"
            package main
            type Point struct {
                x, y, z float64
                name string
            }
        "#,
        );
        let program = parser.parse().unwrap();

        if let Declaration::Type(type_specs) = &program.declarations[0] {
            if let TypeValue::Struct(fields) = &type_specs[0].type_value {
                assert_eq!(fields.len(), 2);

                // First field: x, y, z float64
                assert_eq!(fields[0].names, vec!["x", "y", "z"]);
                assert_eq!(fields[0].field_type, "float64");

                // Second field: name string
                assert_eq!(fields[1].names, vec!["name"]);
                assert_eq!(fields[1].field_type, "string");
            } else {
                panic!("Expected struct type");
            }
        }
    }

    #[test]
    fn test_parse_struct_with_tags() {
        let mut parser = Parser::new(
            r#"
            package main
            type User struct {
                ID int "json:\"id\""
                Name string "json:\"name\" xml:\"name\""
                Email string "json:\"email,omitempty\""
            }
        "#,
        );
        let program = parser.parse().unwrap();

        if let Declaration::Type(type_specs) = &program.declarations[0] {
            if let TypeValue::Struct(fields) = &type_specs[0].type_value {
                assert_eq!(fields.len(), 3);

                // ID field with tag
                assert_eq!(fields[0].names, vec!["ID"]);
                assert_eq!(fields[0].field_type, "int");
                assert_eq!(fields[0].tag, Some("json:\"id\"".to_string()));

                // Name field with tag
                assert_eq!(fields[1].names, vec!["Name"]);
                assert_eq!(fields[1].field_type, "string");
                assert_eq!(
                    fields[1].tag,
                    Some("json:\"name\" xml:\"name\"".to_string())
                );

                // Email field with tag
                assert_eq!(fields[2].names, vec!["Email"]);
                assert_eq!(fields[2].field_type, "string");
                assert_eq!(fields[2].tag, Some("json:\"email,omitempty\"".to_string()));
            } else {
                panic!("Expected struct type");
            }
        }
    }

    #[test]
    fn test_parse_empty_struct() {
        let mut parser = Parser::new(
            r#"
            package main
            type Empty struct {}
        "#,
        );
        let program = parser.parse().unwrap();

        if let Declaration::Type(type_specs) = &program.declarations[0] {
            if let TypeValue::Struct(fields) = &type_specs[0].type_value {
                assert_eq!(fields.len(), 0);
            } else {
                panic!("Expected struct type");
            }
        }
    }

    #[test]
    fn test_parse_struct_literal() {
        let mut parser = Parser::new(
            r#"
            package main
            func main() {
                p := Person{name: "John", age: 30}
            }
        "#,
        );
        let program = parser.parse().unwrap();

        if let Declaration::Function(func) = &program.declarations[0] {
            if let Statement::ShortDeclStmt(names, values) = &func.body.statements[0] {
                assert_eq!(names, &vec!["p"]);
                assert_eq!(values.len(), 1);

                if let Expression::StructLiteral(struct_name, fields) = &values[0] {
                    assert_eq!(struct_name, "Person");
                    assert_eq!(fields.len(), 2);

                    // name field
                    assert_eq!(fields[0].name, "name");
                    if let Expression::StringLiteral(value) = &fields[0].value {
                        assert_eq!(value, "John");
                    } else {
                        panic!("Expected string literal for name field");
                    }

                    // age field
                    assert_eq!(fields[1].name, "age");
                    if let Expression::IntLiteral(value) = &fields[1].value {
                        assert_eq!(*value, 30);
                    } else {
                        panic!("Expected int literal for age field");
                    }
                } else {
                    panic!("Expected struct literal");
                }
            } else {
                panic!("Expected short declaration statement");
            }
        }
    }

    #[test]
    fn test_parse_empty_struct_literal() {
        let mut parser = Parser::new(
            r#"
            package main
            func main() {
                empty := Empty{}
            }
        "#,
        );
        let program = parser.parse().unwrap();

        if let Declaration::Function(func) = &program.declarations[0] {
            if let Statement::ShortDeclStmt(names, values) = &func.body.statements[0] {
                assert_eq!(names, &vec!["empty"]);

                if let Expression::StructLiteral(struct_name, fields) = &values[0] {
                    assert_eq!(struct_name, "Empty");
                    assert_eq!(fields.len(), 0);
                } else {
                    panic!("Expected struct literal");
                }
            }
        }
    }

    #[test]
    fn test_parse_struct_field_access() {
        let mut parser = Parser::new(
            r#"
            package main
            func main() {
                name := person.name
                age := person.address.zipcode
            }
        "#,
        );
        let program = parser.parse().unwrap();

        if let Declaration::Function(func) = &program.declarations[0] {
            // First statement: name := person.name
            if let Statement::ShortDeclStmt(names, values) = &func.body.statements[0] {
                assert_eq!(names, &vec!["name"]);

                if let Expression::FieldAccessExpr(obj, field) = &values[0] {
                    if let Expression::Identifier(obj_name) = &**obj {
                        assert_eq!(obj_name, "person");
                    }
                    assert_eq!(field, "name");
                } else {
                    panic!("Expected field access expression");
                }
            }

            // Second statement: age := person.address.zipcode (chained field access)
            if let Statement::ShortDeclStmt(names, values) = &func.body.statements[1] {
                assert_eq!(names, &vec!["age"]);

                if let Expression::FieldAccessExpr(obj, field) = &values[0] {
                    if let Expression::FieldAccessExpr(inner_obj, inner_field) = &**obj {
                        if let Expression::Identifier(obj_name) = &**inner_obj {
                            assert_eq!(obj_name, "person");
                        }
                        assert_eq!(inner_field, "address");
                    }
                    assert_eq!(field, "zipcode");
                } else {
                    panic!("Expected chained field access expression");
                }
            }
        }
    }

    #[test]
    fn test_parse_struct_assignment() {
        let mut parser = Parser::new(
            r#"
            package main
            func main() {
                person.name = "Jane"
                person.age = 25
            }
        "#,
        );
        let program = parser.parse().unwrap();

        if let Declaration::Function(func) = &program.declarations[0] {
            // First assignment: person.name = "Jane"
            if let Statement::AssignmentStmt(left, right) = &func.body.statements[0] {
                assert_eq!(left.len(), 1);
                assert_eq!(right.len(), 1);

                if let Expression::FieldAccessExpr(obj, field) = &left[0] {
                    if let Expression::Identifier(obj_name) = &**obj {
                        assert_eq!(obj_name, "person");
                    }
                    assert_eq!(field, "name");
                }

                if let Expression::StringLiteral(value) = &right[0] {
                    assert_eq!(value, "Jane");
                }
            }

            // Second assignment: person.age = 25
            if let Statement::AssignmentStmt(left, right) = &func.body.statements[1] {
                if let Expression::FieldAccessExpr(obj, field) = &left[0] {
                    if let Expression::Identifier(obj_name) = &**obj {
                        assert_eq!(obj_name, "person");
                    }
                    assert_eq!(field, "age");
                }

                if let Expression::IntLiteral(value) = &right[0] {
                    assert_eq!(*value, 25);
                }
            }
        }
    }

    #[test]
    fn test_parse_nested_struct_types() {
        let mut parser = Parser::new(
            r#"
            package main
            type Address struct {
                street string
                city string
                zipcode int
            }
            
            type Person struct {
                name string
                age int
                address Address
            }
        "#,
        );
        let program = parser.parse().unwrap();

        assert_eq!(program.declarations.len(), 2);

        // Address struct
        if let Declaration::Type(type_specs) = &program.declarations[0] {
            assert_eq!(type_specs[0].name, "Address");
            if let TypeValue::Struct(fields) = &type_specs[0].type_value {
                assert_eq!(fields.len(), 3);
                assert_eq!(fields[0].names, vec!["street"]);
                assert_eq!(fields[0].field_type, "string");
                assert_eq!(fields[1].names, vec!["city"]);
                assert_eq!(fields[1].field_type, "string");
                assert_eq!(fields[2].names, vec!["zipcode"]);
                assert_eq!(fields[2].field_type, "int");
            }
        }

        // Person struct with Address field
        if let Declaration::Type(type_specs) = &program.declarations[1] {
            assert_eq!(type_specs[0].name, "Person");
            if let TypeValue::Struct(fields) = &type_specs[0].type_value {
                assert_eq!(fields.len(), 3);
                assert_eq!(fields[2].names, vec!["address"]);
                assert_eq!(fields[2].field_type, "Address");
            }
        }
    }

    #[test]
    fn test_parse_interface_type() {
        let mut parser = Parser::new(
            r#"
            package main
            type Writer interface {
                Write(data string) int
                Close() error
            }
        "#,
        );
        let program = parser.parse().unwrap();

        if let Declaration::Type(type_specs) = &program.declarations[0] {
            assert_eq!(type_specs.len(), 1);
            assert_eq!(type_specs[0].name, "Writer");

            if let TypeValue::Interface(methods) = &type_specs[0].type_value {
                assert_eq!(methods.len(), 2);

                // Write method
                assert_eq!(methods[0].name, "Write");
                assert_eq!(methods[0].parameters.len(), 1);
                assert_eq!(methods[0].parameters[0].name, "data");
                assert_eq!(methods[0].parameters[0].param_type, "string");
                assert_eq!(methods[0].return_type, Some("int".to_string()));

                // Close method
                assert_eq!(methods[1].name, "Close");
                assert_eq!(methods[1].parameters.len(), 0);
                assert_eq!(methods[1].return_type, Some("error".to_string()));
            } else {
                panic!("Expected interface type");
            }
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn test_parse_complex_struct_usage() {
        let mut parser = Parser::new(
            r#"
            package main
            
            type Point struct {
                x, y float64
            }
            
            func main() {
                p1 := Point{x: 1.0, y: 2.0}
                p2 := Point{}
                distance := p1.x - p2.x
                p1.y = p1.y + 5.0
            }
        "#,
        );
        let program = parser.parse().unwrap();

        assert_eq!(program.declarations.len(), 2);

        // Verify Point struct declaration
        if let Declaration::Type(type_specs) = &program.declarations[0] {
            assert_eq!(type_specs[0].name, "Point");
        }

        // Verify function with struct usage
        if let Declaration::Function(func) = &program.declarations[1] {
            assert_eq!(func.body.statements.len(), 4);

            // p1 := Point{x: 1.0, y: 2.0}
            if let Statement::ShortDeclStmt(_, values) = &func.body.statements[0] {
                if let Expression::StructLiteral(name, fields) = &values[0] {
                    assert_eq!(name, "Point");
                    assert_eq!(fields.len(), 2);
                }
            }

            // p2 := Point{}
            if let Statement::ShortDeclStmt(_, values) = &func.body.statements[1] {
                if let Expression::StructLiteral(name, fields) = &values[0] {
                    assert_eq!(name, "Point");
                    assert_eq!(fields.len(), 0);
                }
            }

            // distance := p1.x - p2.x
            if let Statement::ShortDeclStmt(names, values) = &func.body.statements[2] {
                assert_eq!(names, &vec!["distance"]);
                if let Expression::BinaryExpr(left, BinaryOp::Sub, right) = &values[0] {
                    // Verify both sides are field access expressions
                    if let Expression::FieldAccessExpr(_, field) = &**left {
                        assert_eq!(field, "x");
                    }
                    if let Expression::FieldAccessExpr(_, field) = &**right {
                        assert_eq!(field, "x");
                    }
                }
            }

            // p1.y = p1.y + 5.0
            if let Statement::AssignmentStmt(left, right) = &func.body.statements[3] {
                if let Expression::FieldAccessExpr(_, field) = &left[0] {
                    assert_eq!(field, "y");
                }
                if let Expression::BinaryExpr(_, BinaryOp::Add, _) = &right[0] {
                    // Correct binary expression
                }
            }
        }
    }
}

// #[test]
// fn test_parse_interface_type() {
//     let mut parser = Parser::new(r#"
//         package main
//         type Writer interface {
//             Write(data string) int
//         }
//     "#);
//     let program = parser.parse().unwrap();

//     if let Declaration::Type(type_specs) = &program.declarations[0] {
//         assert_eq!(type_specs.len(), 1);
//         assert_eq!(type_specs[0].name, "Writer");

//         if let TypeValue::Interface(methods) = &type_specs[0].type_value {
//             assert_eq!(methods.len(), 1);
//             assert_eq!(methods[0].name, "Write");
//             assert_eq!(methods[0].parameters.len(), 1);
//             assert_eq!(methods[0].parameters[0].name, "data");
//             assert_eq!(methods[0].parameters[0].param_type, "string");
