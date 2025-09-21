use goiaba::parser::Parser;

fn main() {
    let source = r#"
    package main

    import (
        "fmt"
        "strings"
    )

    func main() {
        greeting := "Hello, Go!"
        fmt.Println(greeting)
        
        x := 42
        if x > 40 {
            fmt.Println("Greater than 40")
        } else {
            fmt.Println("Not greater than 40")
        }
        
        for i := 0; i < 5; i++ {
            fmt.Println(i)
        }
    }
    "#;

    let mut parser = Parser::new(source);
    match parser.parse() {
        Ok(program) => println!("Successfully parsed program: {:?}", program),
        Err(error) => println!("Error parsing program: {}", error),
    }
}
