// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a GPL-3.0
// license that can be found in the LICENSE file.

// Copyright 2022 The Goscript Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//! This crate is part of the Goscript project. Please refer to <https://goscript.dev> for more information.
//!
//! It's a port of the the parser from the Go standard library <https://github.com/golang/go/tree/release-branch.go1.12/src/go/parser>
//!  
//! # Usage:
//! ```
//! fn parse_file() {
//!     let source = "package main ...";
//!     let mut fs = goiaba::parser::FileSet::new();
//!     let o = &mut goiaba::parser::AstObjects::new();
//!     let el = &mut goiaba::parser::ErrorList::new();
//!     let (p, _) = goiaba::parser::parse_file(o, &mut fs, el, "./main.go", source, false);
//!     print!("{}", p.get_errors());
//! }
//! ```
//!
//! # Feature
//! - `btree_map`: Make it use BTreeMap instead of HashMap
//!

pub mod errors;
mod map;
pub mod objects;
pub mod parser;
pub mod position;
mod scanner;
#[cfg(test)]
mod tests;
mod token;

pub mod ast;
pub mod scope;
pub mod visitor;

pub use errors::*;

pub use objects::*;
pub use parser::Parser;
pub use position::*;
pub use token::*;

pub fn parse_file<'a>(
    o: &'a mut AstObjects,
    fs: &'a mut FileSet,
    el: &'a ErrorList,
    name: &str,
    src: &'a str,
    trace: bool,
) -> (parser::Parser<'a>, Option<ast::File>) {
    let f = fs.add_file(name.to_string(), None, src.chars().count());
    let mut p = parser::Parser::new(o, f, el, src, trace);
    let file = p.parse_file();
    (p, file)
}

// Simple function to parse Go source code from a string
///
/// This function handles all the internal setup (FileSet, ErrorList, etc.)
/// and returns the parsed AST along with any errors.
///
/// # Arguments
/// * `src` - Go source code as a string
/// * `trace` - Whether to enable parser tracing (for debugging)
///
/// # Returns
/// * `Result<(AstObjects, ast::File), String>` - Parsed AST or error message
pub fn parse_string(src: &str, trace: bool) -> Result<(AstObjects, ast::File), String> {
    let mut ast_objects = AstObjects::new();
    let mut file_set = FileSet::new();
    let error_list = ErrorList::new();

    let file = file_set.add_file("main.go".to_string(), None, src.len());
    let mut parser = Parser::new(&mut ast_objects, file, &error_list, src, trace);

    match parser.parse_file() {
        Some(parsed_file) => {
            if error_list.len() > 0 {
                error_list.sort();
                Err(format!("Parse errors: {}", error_list))
            } else {
                Ok((ast_objects, parsed_file))
            }
        }
        None => {
            error_list.sort();
            Err(format!("Failed to parse: {}", error_list))
        }
    }
}

/// Even simpler version that just returns the File AST
pub fn parse_str(src: &str) -> Result<(AstObjects, ast::File), String> {
    parse_string(src, false)
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_lexer_increment_decrement() {
//         let mut lexer = Lexer::new("++ -- += -=");

//         assert_eq!(lexer.next_token().token_type, TokenType::Increment);
//         assert_eq!(lexer.next_token().token_type, TokenType::Decrement);
//         assert_eq!(lexer.next_token().token_type, TokenType::PlusAssign);
//         assert_eq!(lexer.next_token().token_type, TokenType::MinusAssign);
//     }

//     #[test]
//     fn test_parse_increment_decrement() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             func main() {
//                 i++
//                 j--
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         if let Declaration::Function(func) = &program.declarations[0] {
//             if let Statement::IncrementStmt(expr) = &func.body.statements[0] {
//                 if let Expression::Identifier(name) = expr {
//                     assert_eq!(name, "i");
//                 } else {
//                     panic!("Expected identifier i");
//                 }
//             } else {
//                 panic!("Expected increment statement");
//             }

//             if let Statement::DecrementStmt(expr) = &func.body.statements[1] {
//                 if let Expression::Identifier(name) = expr {
//                     assert_eq!(name, "j");
//                 } else {
//                     panic!("Expected identifier j");
//                 }
//             } else {
//                 panic!("Expected decrement statement");
//             }
//         }
//     }

//     #[test]
//     fn test_parse_for_loop_with_increment() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             func main() {
//                 for i := 0; i < 10; i++ {
//                     return
//                 }
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         if let Declaration::Function(func) = &program.declarations[0] {
//             if let Statement::ForStmt(init, condition, post, _block) = &func.body.statements[0] {
//                 assert!(init.is_some());
//                 assert!(condition.is_some());

//                 if let Some(post_stmt) = post {
//                     if let Statement::IncrementStmt(expr) = &**post_stmt {
//                         if let Expression::Identifier(name) = expr {
//                             assert_eq!(name, "i");
//                         } else {
//                             panic!("Expected identifier i in increment");
//                         }
//                     } else {
//                         panic!("Expected increment statement in post");
//                     }
//                 } else {
//                     panic!("Expected post statement");
//                 }
//             } else {
//                 panic!("Expected for statement");
//             }
//         }
//     }

//     #[test]
//     fn test_lexer_keywords() {
//         let mut lexer = Lexer::new("package main func if else");

//         assert_eq!(lexer.next_token().token_type, TokenType::Package);
//         assert_eq!(lexer.next_token().token_type, TokenType::Identifier);
//         assert_eq!(lexer.next_token().token_type, TokenType::Func);
//         assert_eq!(lexer.next_token().token_type, TokenType::If);
//         assert_eq!(lexer.next_token().token_type, TokenType::Else);
//     }

//     #[test]
//     fn test_lexer_operators() {
//         let mut lexer = Lexer::new("+ - * / % == != < > <= >= && || ! = :=");

//         assert_eq!(lexer.next_token().token_type, TokenType::Plus);
//         assert_eq!(lexer.next_token().token_type, TokenType::Minus);
//         assert_eq!(lexer.next_token().token_type, TokenType::Asterisk);
//         assert_eq!(lexer.next_token().token_type, TokenType::Slash);
//         assert_eq!(lexer.next_token().token_type, TokenType::Percent);
//         assert_eq!(lexer.next_token().token_type, TokenType::Equal);
//         assert_eq!(lexer.next_token().token_type, TokenType::NotEqual);
//         assert_eq!(lexer.next_token().token_type, TokenType::LessThan);
//         assert_eq!(lexer.next_token().token_type, TokenType::GreaterThan);
//         assert_eq!(lexer.next_token().token_type, TokenType::LessEqual);
//         assert_eq!(lexer.next_token().token_type, TokenType::GreaterEqual);
//         assert_eq!(lexer.next_token().token_type, TokenType::And);
//         assert_eq!(lexer.next_token().token_type, TokenType::Or);
//         assert_eq!(lexer.next_token().token_type, TokenType::Not);
//         assert_eq!(lexer.next_token().token_type, TokenType::Assign);
//         assert_eq!(lexer.next_token().token_type, TokenType::ShortDeclare);
//     }

//     #[test]
//     fn test_lexer_literals() {
//         let mut lexer = Lexer::new(r#"123 3.14 "hello" 'a'"#);

//         let token = lexer.next_token();
//         assert_eq!(token.token_type, TokenType::IntLiteral);
//         assert_eq!(token.lexeme, "123");

//         let token = lexer.next_token();
//         assert_eq!(token.token_type, TokenType::FloatLiteral);
//         assert_eq!(token.lexeme, "3.14");

//         let token = lexer.next_token();
//         assert_eq!(token.token_type, TokenType::StringLiteral);
//         assert_eq!(token.lexeme, "hello");

//         let token = lexer.next_token();
//         assert_eq!(token.token_type, TokenType::CharLiteral);
//         assert_eq!(token.lexeme, "a");
//     }

//     #[test]
//     fn test_lexer_comments() {
//         let mut lexer = Lexer::new("// This is a comment\npackage main");

//         let token = lexer.next_token();
//         assert_eq!(token.token_type, TokenType::Package);
//     }

//     #[test]
//     fn test_lexer_block_comments() {
//         let mut lexer = Lexer::new("/* This is a\n   block comment */\nfunc main");

//         let token = lexer.next_token();
//         assert_eq!(token.token_type, TokenType::Func);
//     }

//     #[test]
//     fn test_parse_simple_package() {
//         let mut parser = Parser::new("package main");
//         let program = parser.parse().unwrap();

//         assert_eq!(program.package, "main");
//         assert!(program.imports.is_empty());
//         assert!(program.declarations.is_empty());
//     }

//     #[test]
//     fn test_parse_package_with_imports() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             import "fmt"
//             import "os"
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         assert_eq!(program.package, "main");
//         assert_eq!(program.imports.len(), 2);
//         assert_eq!(program.imports[0], "fmt");
//         assert_eq!(program.imports[1], "os");
//     }

//     #[test]
//     fn test_parse_grouped_imports() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             import (
//                 "fmt"
//                 "os"
//             )
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         assert_eq!(program.package, "main");
//         assert_eq!(program.imports.len(), 2);
//         assert_eq!(program.imports[0], "fmt");
//         assert_eq!(program.imports[1], "os");
//     }

//     #[test]
//     fn test_parse_simple_function() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             func main() {
//                 return
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         assert_eq!(program.declarations.len(), 1);
//         if let Declaration::Function(func) = &program.declarations[0] {
//             assert_eq!(func.name, "main");
//             assert!(func.parameters.is_empty());
//             assert!(func.return_type.is_none());
//             assert_eq!(func.body.statements.len(), 1);

//             if let Statement::ReturnStmt(None) = &func.body.statements[0] {
//                 // Correct
//             } else {
//                 panic!("Expected return statement");
//             }
//         } else {
//             panic!("Expected function declaration");
//         }
//     }

//     #[test]
//     fn test_parse_function_with_parameters() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             func add(x int, y int) int {
//                 return x + y
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         assert_eq!(program.declarations.len(), 1);
//         if let Declaration::Function(func) = &program.declarations[0] {
//             assert_eq!(func.name, "add");
//             assert_eq!(func.parameters.len(), 2);
//             assert_eq!(func.parameters[0].name, "x");
//             assert_eq!(func.parameters[0].param_type, "int");
//             assert_eq!(func.parameters[1].name, "y");
//             assert_eq!(func.parameters[1].param_type, "int");
//             assert_eq!(func.return_type, Some("int".to_string()));
//         } else {
//             panic!("Expected function declaration");
//         }
//     }

//     #[test]
//     fn test_parse_variable_declaration() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             var x int = 42
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         assert_eq!(program.declarations.len(), 1);
//         if let Declaration::Variable(var_specs) = &program.declarations[0] {
//             assert_eq!(var_specs.len(), 1);
//             assert_eq!(var_specs[0].names, vec!["x"]);
//             assert_eq!(var_specs[0].var_type, Some("int".to_string()));

//             if let Some(values) = &var_specs[0].values {
//                 assert_eq!(values.len(), 1);
//                 if let Expression::IntLiteral(42) = &values[0] {
//                     // Correct
//                 } else {
//                     panic!("Expected int literal 42");
//                 }
//             } else {
//                 panic!("Expected variable value");
//             }
//         } else {
//             panic!("Expected variable declaration");
//         }
//     }

//     #[test]
//     fn test_parse_short_declaration() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             func main() {
//                 x := 42
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         if let Declaration::Function(func) = &program.declarations[0] {
//             if let Statement::ShortDeclStmt(names, values) = &func.body.statements[0] {
//                 assert_eq!(names, &vec!["x"]);
//                 assert_eq!(values.len(), 1);
//                 if let Expression::IntLiteral(42) = &values[0] {
//                     // Correct
//                 } else {
//                     panic!("Expected int literal 42");
//                 }
//             } else {
//                 panic!("Expected short declaration statement");
//             }
//         }
//     }

//     #[test]
//     fn test_parse_if_statement() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             func main() {
//                 if x > 0 {
//                     return x
//                 }
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         if let Declaration::Function(func) = &program.declarations[0] {
//             if let Statement::IfStmt(condition, _if_block, _else_branch) = &func.body.statements[0]
//             {
//                 if let Expression::BinaryExpr(_, BinaryOp::Gt, _) = condition {
//                     // Correct
//                 } else {
//                     panic!("Expected binary expression with > operator");
//                 }
//             } else {
//                 panic!("Expected if statement");
//             }
//         }
//     }

//     #[test]
//     fn test_parse_for_loop() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             func main() {
//                 for i := 0; i < 10; i = i + 1 {
//                     return
//                 }
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         if let Declaration::Function(func) = &program.declarations[0] {
//             if let Statement::ForStmt(init, condition, post, _block) = &func.body.statements[0] {
//                 assert!(init.is_some());
//                 assert!(condition.is_some());
//                 assert!(post.is_some());
//             } else {
//                 panic!("Expected for statement");
//             }
//         }
//     }

//     #[test]
//     fn test_parse_struct_type() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             type Person struct {
//                 name string
//                 age int
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         if let Declaration::Type(type_specs) = &program.declarations[0] {
//             assert_eq!(type_specs.len(), 1);
//             assert_eq!(type_specs[0].name, "Person");

//             if let TypeValue::Struct(fields) = &type_specs[0].type_value {
//                 assert_eq!(fields.len(), 2);
//                 assert_eq!(fields[0].names, vec!["name"]);
//                 assert_eq!(fields[0].field_type, "string");
//                 assert_eq!(fields[1].names, vec!["age"]);
//                 assert_eq!(fields[1].field_type, "int");
//             } else {
//                 panic!("Expected struct type");
//             }
//         } else {
//             panic!("Expected type declaration");
//         }
//     }

//     #[test]
//     fn test_parse_struct_with_multiple_field_names() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             type Point struct {
//                 x, y, z float64
//                 name string
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         if let Declaration::Type(type_specs) = &program.declarations[0] {
//             if let TypeValue::Struct(fields) = &type_specs[0].type_value {
//                 assert_eq!(fields.len(), 2);

//                 // First field: x, y, z float64
//                 assert_eq!(fields[0].names, vec!["x", "y", "z"]);
//                 assert_eq!(fields[0].field_type, "float64");

//                 // Second field: name string
//                 assert_eq!(fields[1].names, vec!["name"]);
//                 assert_eq!(fields[1].field_type, "string");
//             } else {
//                 panic!("Expected struct type");
//             }
//         }
//     }

//     #[test]
//     fn test_parse_struct_with_tags() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             type User struct {
//                 ID int "json:\"id\""
//                 Name string "json:\"name\" xml:\"name\""
//                 Email string "json:\"email,omitempty\""
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         if let Declaration::Type(type_specs) = &program.declarations[0] {
//             if let TypeValue::Struct(fields) = &type_specs[0].type_value {
//                 assert_eq!(fields.len(), 3);

//                 // ID field with tag
//                 assert_eq!(fields[0].names, vec!["ID"]);
//                 assert_eq!(fields[0].field_type, "int");
//                 assert_eq!(fields[0].tag, Some("json:\"id\"".to_string()));

//                 // Name field with tag
//                 assert_eq!(fields[1].names, vec!["Name"]);
//                 assert_eq!(fields[1].field_type, "string");
//                 assert_eq!(
//                     fields[1].tag,
//                     Some("json:\"name\" xml:\"name\"".to_string())
//                 );

//                 // Email field with tag
//                 assert_eq!(fields[2].names, vec!["Email"]);
//                 assert_eq!(fields[2].field_type, "string");
//                 assert_eq!(fields[2].tag, Some("json:\"email,omitempty\"".to_string()));
//             } else {
//                 panic!("Expected struct type");
//             }
//         }
//     }

//     #[test]
//     fn test_parse_empty_struct() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             type Empty struct {}
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         if let Declaration::Type(type_specs) = &program.declarations[0] {
//             if let TypeValue::Struct(fields) = &type_specs[0].type_value {
//                 assert_eq!(fields.len(), 0);
//             } else {
//                 panic!("Expected struct type");
//             }
//         }
//     }

//     #[test]
//     fn test_parse_struct_literal() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             func main() {
//                 p := Person{name: "John", age: 30}
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         if let Declaration::Function(func) = &program.declarations[0] {
//             if let Statement::ShortDeclStmt(names, values) = &func.body.statements[0] {
//                 assert_eq!(names, &vec!["p"]);
//                 assert_eq!(values.len(), 1);

//                 if let Expression::StructLiteral(struct_name, fields) = &values[0] {
//                     assert_eq!(struct_name, "Person");
//                     assert_eq!(fields.len(), 2);

//                     // name field
//                     assert_eq!(fields[0].name, "name");
//                     if let Expression::StringLiteral(value) = &fields[0].value {
//                         assert_eq!(value, "John");
//                     } else {
//                         panic!("Expected string literal for name field");
//                     }

//                     // age field
//                     assert_eq!(fields[1].name, "age");
//                     if let Expression::IntLiteral(value) = &fields[1].value {
//                         assert_eq!(*value, 30);
//                     } else {
//                         panic!("Expected int literal for age field");
//                     }
//                 } else {
//                     panic!("Expected struct literal");
//                 }
//             } else {
//                 panic!("Expected short declaration statement");
//             }
//         }
//     }

//     #[test]
//     fn test_parse_empty_struct_literal() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             func main() {
//                 empty := Empty{}
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         if let Declaration::Function(func) = &program.declarations[0] {
//             if let Statement::ShortDeclStmt(names, values) = &func.body.statements[0] {
//                 assert_eq!(names, &vec!["empty"]);

//                 if let Expression::StructLiteral(struct_name, fields) = &values[0] {
//                     assert_eq!(struct_name, "Empty");
//                     assert_eq!(fields.len(), 0);
//                 } else {
//                     panic!("Expected struct literal");
//                 }
//             }
//         }
//     }

//     #[test]
//     fn test_parse_struct_field_access() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             func main() {
//                 name := person.name
//                 age := person.address.zipcode
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         if let Declaration::Function(func) = &program.declarations[0] {
//             // First statement: name := person.name
//             if let Statement::ShortDeclStmt(names, values) = &func.body.statements[0] {
//                 assert_eq!(names, &vec!["name"]);

//                 if let Expression::FieldAccessExpr(obj, field) = &values[0] {
//                     if let Expression::Identifier(obj_name) = &**obj {
//                         assert_eq!(obj_name, "person");
//                     }
//                     assert_eq!(field, "name");
//                 } else {
//                     panic!("Expected field access expression");
//                 }
//             }

//             // Second statement: age := person.address.zipcode (chained field access)
//             if let Statement::ShortDeclStmt(names, values) = &func.body.statements[1] {
//                 assert_eq!(names, &vec!["age"]);

//                 if let Expression::FieldAccessExpr(obj, field) = &values[0] {
//                     if let Expression::FieldAccessExpr(inner_obj, inner_field) = &**obj {
//                         if let Expression::Identifier(obj_name) = &**inner_obj {
//                             assert_eq!(obj_name, "person");
//                         }
//                         assert_eq!(inner_field, "address");
//                     }
//                     assert_eq!(field, "zipcode");
//                 } else {
//                     panic!("Expected chained field access expression");
//                 }
//             }
//         }
//     }

//     #[test]
//     fn test_parse_struct_assignment() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             func main() {
//                 person.name = "Jane"
//                 person.age = 25
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         if let Declaration::Function(func) = &program.declarations[0] {
//             // First assignment: person.name = "Jane"
//             if let Statement::AssignmentStmt(left, right) = &func.body.statements[0] {
//                 assert_eq!(left.len(), 1);
//                 assert_eq!(right.len(), 1);

//                 if let Expression::FieldAccessExpr(obj, field) = &left[0] {
//                     if let Expression::Identifier(obj_name) = &**obj {
//                         assert_eq!(obj_name, "person");
//                     }
//                     assert_eq!(field, "name");
//                 }

//                 if let Expression::StringLiteral(value) = &right[0] {
//                     assert_eq!(value, "Jane");
//                 }
//             }

//             // Second assignment: person.age = 25
//             if let Statement::AssignmentStmt(left, right) = &func.body.statements[1] {
//                 if let Expression::FieldAccessExpr(obj, field) = &left[0] {
//                     if let Expression::Identifier(obj_name) = &**obj {
//                         assert_eq!(obj_name, "person");
//                     }
//                     assert_eq!(field, "age");
//                 }

//                 if let Expression::IntLiteral(value) = &right[0] {
//                     assert_eq!(*value, 25);
//                 }
//             }
//         }
//     }

//     #[test]
//     fn test_parse_nested_struct_types() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             type Address struct {
//                 street string
//                 city string
//                 zipcode int
//             }

//             type Person struct {
//                 name string
//                 age int
//                 address Address
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         assert_eq!(program.declarations.len(), 2);

//         // Address struct
//         if let Declaration::Type(type_specs) = &program.declarations[0] {
//             assert_eq!(type_specs[0].name, "Address");
//             if let TypeValue::Struct(fields) = &type_specs[0].type_value {
//                 assert_eq!(fields.len(), 3);
//                 assert_eq!(fields[0].names, vec!["street"]);
//                 assert_eq!(fields[0].field_type, "string");
//                 assert_eq!(fields[1].names, vec!["city"]);
//                 assert_eq!(fields[1].field_type, "string");
//                 assert_eq!(fields[2].names, vec!["zipcode"]);
//                 assert_eq!(fields[2].field_type, "int");
//             }
//         }

//         // Person struct with Address field
//         if let Declaration::Type(type_specs) = &program.declarations[1] {
//             assert_eq!(type_specs[0].name, "Person");
//             if let TypeValue::Struct(fields) = &type_specs[0].type_value {
//                 assert_eq!(fields.len(), 3);
//                 assert_eq!(fields[2].names, vec!["address"]);
//                 assert_eq!(fields[2].field_type, "Address");
//             }
//         }
//     }

//     #[test]
//     fn test_parse_interface_type() {
//         let mut parser = Parser::new(
//             r#"
//             package main
//             type Writer interface {
//                 Write(data string) int
//                 Close() error
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         if let Declaration::Type(type_specs) = &program.declarations[0] {
//             assert_eq!(type_specs.len(), 1);
//             assert_eq!(type_specs[0].name, "Writer");

//             if let TypeValue::Interface(methods) = &type_specs[0].type_value {
//                 assert_eq!(methods.len(), 2);

//                 // Write method
//                 assert_eq!(methods[0].name, "Write");
//                 assert_eq!(methods[0].parameters.len(), 1);
//                 assert_eq!(methods[0].parameters[0].name, "data");
//                 assert_eq!(methods[0].parameters[0].param_type, "string");
//                 assert_eq!(methods[0].return_type, Some("int".to_string()));

//                 // Close method
//                 assert_eq!(methods[1].name, "Close");
//                 assert_eq!(methods[1].parameters.len(), 0);
//                 assert_eq!(methods[1].return_type, Some("error".to_string()));
//             } else {
//                 panic!("Expected interface type");
//             }
//         } else {
//             panic!("Expected type declaration");
//         }
//     }

//     #[test]
//     fn test_parse_complex_struct_usage() {
//         let mut parser = Parser::new(
//             r#"
//             package main

//             type Point struct {
//                 x, y float64
//             }

//             func main() {
//                 p1 := Point{x: 1.0, y: 2.0}
//                 p2 := Point{}
//                 distance := p1.x - p2.x
//                 p1.y = p1.y + 5.0
//             }
//         "#,
//         );
//         let program = parser.parse().unwrap();

//         assert_eq!(program.declarations.len(), 2);

//         // Verify Point struct declaration
//         if let Declaration::Type(type_specs) = &program.declarations[0] {
//             assert_eq!(type_specs[0].name, "Point");
//         }

//         // Verify function with struct usage
//         if let Declaration::Function(func) = &program.declarations[1] {
//             assert_eq!(func.body.statements.len(), 4);

//             // p1 := Point{x: 1.0, y: 2.0}
//             if let Statement::ShortDeclStmt(_, values) = &func.body.statements[0] {
//                 if let Expression::StructLiteral(name, fields) = &values[0] {
//                     assert_eq!(name, "Point");
//                     assert_eq!(fields.len(), 2);
//                 }
//             }

//             // p2 := Point{}
//             if let Statement::ShortDeclStmt(_, values) = &func.body.statements[1] {
//                 if let Expression::StructLiteral(name, fields) = &values[0] {
//                     assert_eq!(name, "Point");
//                     assert_eq!(fields.len(), 0);
//                 }
//             }

//             // distance := p1.x - p2.x
//             if let Statement::ShortDeclStmt(names, values) = &func.body.statements[2] {
//                 assert_eq!(names, &vec!["distance"]);
//                 if let Expression::BinaryExpr(left, BinaryOp::Sub, right) = &values[0] {
//                     // Verify both sides are field access expressions
//                     if let Expression::FieldAccessExpr(_, field) = &**left {
//                         assert_eq!(field, "x");
//                     }
//                     if let Expression::FieldAccessExpr(_, field) = &**right {
//                         assert_eq!(field, "x");
//                     }
//                 }
//             }

//             // p1.y = p1.y + 5.0
//             if let Statement::AssignmentStmt(left, right) = &func.body.statements[3] {
//                 if let Expression::FieldAccessExpr(_, field) = &left[0] {
//                     assert_eq!(field, "y");
//                 }
//                 if let Expression::BinaryExpr(_, BinaryOp::Add, _) = &right[0] {
//                     // Correct binary expression
//                 }
//             }
//         }
//     }
// }

// // #[test]
// // fn test_parse_interface_type() {
// //     let mut parser = Parser::new(r#"
// //         package main
// //         type Writer interface {
// //             Write(data string) int
// //         }
// //     "#);
// //     let program = parser.parse().unwrap();

// //     if let Declaration::Type(type_specs) = &program.declarations[0] {
// //         assert_eq!(type_specs.len(), 1);
// //         assert_eq!(type_specs[0].name, "Writer");

// //         if let TypeValue::Interface(methods) = &type_specs[0].type_value {
// //             assert_eq!(methods.len(), 1);
// //             assert_eq!(methods[0].name, "Write");
// //             assert_eq!(methods[0].parameters.len(), 1);
// //             assert_eq!(methods[0].parameters[0].name, "data");
// //             assert_eq!(methods[0].parameters[0].param_type, "string");
