# Goiaba

Experimental parser and WASM-based Go compiler.

## Usage

```rust
use goiaba::wasm::compiler::compile_str;

let go_source = r#"
    package main

    //export add
    func add(x int, y int) int {
        return x + y
    }
"#;

let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");
```

## Todo

- [x] Parses Go source code into an Abstract Syntax Tree (AST)
- [x] Translates Go constructs to WebAssembly representations
- [x] Compiles to WebAssembly bytecode
- [x] Supports function definitions with parameter and return types
- [x] Handles variable declarations and assignments
- [x] Supports control flow statements (if/else, loops)
- [x] Generates exportable WASM functions
- [x] Supports arithmetic and comparison operations
- [x] Handles increment/decrement operators
- [ ] Support for struct types
- [ ] Support for arrays and slices
- [ ] Implement switch statements
- [ ] Add support for string literals
- [ ] Implement pointer types
- [ ] Add garbage collection support
- [ ] Support for goroutines and channels
- [ ] Implement more standard library functions
- [ ] Add support for package imports
- [ ] Improve error reporting and diagnostics
- [ ] Optimize generated WASM code
- [ ] Add support for floating-point operations
- [ ] Cli support `goiaba main.go`
- [ ] Cli support produces the js with wasm-bindgen

## License

GPL-3.0