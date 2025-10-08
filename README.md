# Goiaba

Experimental Go parser and compiler.

## WebAssembly Compilation Usage

```rust
use goiaba::wasm::compiler::compile_str;
use wasmtime::{Engine, Instance, Module, Store};

fn main() {
    let go_source = r#"
        package main
        
        //export add
        func add(x int, y int) int {
            return x + y
        }
    "#;

    let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

    // Create a WASM runtime
    let engine = Engine::default();
    let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
    let mut store = Store::new(&engine, ());

    // Instantiate the module
    let instance =
        Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

    // Get the exported function
    let add_func = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "add")
        .expect("Failed to get 'add' function");

    // Call the function
    let result = add_func
        .call(&mut store, (5, 3))
        .expect("Failed to call 'add' function");

    // Verify the result
    assert_eq!(result, 8);
}
```

## CLI Usage

```cli
goiaba main.go -o main.wasm
goiaba input.go --output output.wasm --verbose

# Generate a complete web project
goiaba main.go -w ./web-project

# Compile with custom output and web generation  
goiaba calculator.go -o calc.wasm -w ./demo --verbose
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
- [x] Cli support `goiaba main.go`
- [ ] Cli support produces the js with wasm-bindgen
- [ ] Cli support produces the rust (more info soon)
- [ ] Cli support produces the zig (more info soon)
- [ ] Add LLVM-IR target for compilation

## License

BSD-3-Clause - Raphael Amorim