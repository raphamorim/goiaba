// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a GPL-3.0
// license that can be found in the LICENSE file.

#[cfg(test)]
mod tests {
    use goiaba::parser::Expression::{CallExpr, FieldAccessExpr, Identifier, StringLiteral};
    use goiaba::parser::Statement::ExpressionStmt;
    use goiaba::parser::{Block, Declaration, Function, Parser, Program};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_hello_world() {
        let source = r#"
            package main

            import (
                "fmt"
            )

            func main() {
                fmt.Println("Hello World!")
            }
            "#;

        let mut parser = Parser::new(source);
        let program = parser.parse().expect("Error parsing program");
        assert_eq!(
            program,
            Program {
                package: "main".to_string(),
                imports: vec![String::from("fmt")],
                declarations: vec![Declaration::Function(Function {
                    name: String::from("main"),
                    parameters: vec![],
                    return_type: None,
                    body: Block {
                        statements: vec![ExpressionStmt(CallExpr(
                            Box::new(FieldAccessExpr(
                                Box::new(Identifier(String::from("fmt"))),
                                String::from("Println")
                            )),
                            vec![StringLiteral(String::from("Hello World!"))]
                        ))]
                    }
                })],
            }
        );
    }
}
