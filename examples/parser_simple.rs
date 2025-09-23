use goiaba::parser::parse_str;

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

    match parse_str(source) {
        Ok(f) => println!("Successfully parsed file"),
        Err(err) => println!("Error parsing file: {}", err),
    }
}
