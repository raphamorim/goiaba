> [!WARNING]
> This is a work in progress! For updates on the ongoing development, follow me on [Twitter](https://x.com/raphamorims) or [Bluesky](https://bsky.app/profile/raphamorim.bsky.social), or watch the repository.

# Goiaba

An experimental Go parser and WebAssembly compiler written in Rust. Goiaba translates Go source code into WebAssembly bytecode, enabling Go programs to run in web browsers and other WebAssembly environments.

Reasons why I am building it:

- Create a complete bindgen support for JavaScript (web/nodejs/bun/deno)
- Study of [gollvm](https://go.googlesource.com/gollvm/) project
- It will be used by a code editor I am writing in Rust called [Boo](https://raphamorim.io/building-boo-code-editor-1/)
- Well, mostly learn tbh

## Features

- Parse Go source code into an Abstract Syntax Tree (AST)
- Compile Go functions to WebAssembly modules
- Support for fundamental Go language features (functions, control flow, arithmetic)
- Export Go functions for use in JavaScript/WebAssembly environments
- Export Go functions for use in Rust through C ABI
- Export Go functions for use in Zig through C ABI
- Command-line interface for compilation
- Programmatic API for integration into Rust projects

## Installation

### As a CLI Tool

```bash
cargo install goiaba
```

### As a Library

Add to your `Cargo.toml`:

```toml
[dependencies]
goiaba = "*"
```

## Usage

### Command Line Interface

Basic compilation:

```bash
goiaba main.go -o main.wasm
```

Compile with verbose output:

```bash
goiaba input.go --output output.wasm --verbose
```

Generate a complete nodejs/bun/deno library from Go source code:

```bash
goiaba main.go --lib
```

Generate a complete web project with HTML and JavaScript:

```bash
goiaba main.go --web ./web-project
```

### Library Usage

#### Basic Compilation

```rust
use goiaba::wasm::compiler::compile_str;

fn main() {
    let go_source = r#"
        package main

        //export add
        func add(x int, y int) int {
            return x + y
        }
    "#;

    let wasm_bytes = compile_str(go_source)
        .expect("Failed to compile Go to WASM");

    // Write to file or use with a WASM runtime
    std::fs::write("output.wasm", wasm_bytes)
        .expect("Failed to write WASM file");
}
```

#### Executing Compiled WASM

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

    let wasm_bytes = compile_str(go_source)
        .expect("Failed to compile Go to WASM");

    // Create a WASM runtime
    let engine = Engine::default();
    let module = Module::from_binary(&engine, &wasm_bytes)
        .expect("Failed to load WASM module");
    let mut store = Store::new(&engine, ());

    // Instantiate the module
    let instance = Instance::new(&mut store, &module, &[])
        .expect("Failed to instantiate module");

    // Get the exported function
    let add_func = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "add")
        .expect("Failed to get 'add' function");

    // Call the function
    let result = add_func
        .call(&mut store, (5, 3))
        .expect("Failed to call 'add' function");

    assert_eq!(result, 8);
}
```

#### Parsing Go Source Code

```rust
use goiaba::parser::parse_str;

fn main() {
    let source = r#"
        package main

        func fibonacci(n int) int {
            if n <= 1 {
                return n
            }
            return fibonacci(n-1) + fibonacci(n-2)
        }
    "#;

    match parse_str(source) {
        Ok((objects, file)) => {
            println!("Successfully parsed Go source code");
            // Access AST nodes through objects and file
        }
        Err(err) => {
            eprintln!("Parse error: {}", err);
        }
    }
}
```

## Export Directive

To make Go functions callable from WebAssembly, use the `//export` directive:

```go
//export function_name
func function_name(param1 int, param2 int) int {
    return param1 + param2
}
```

The exported name will be used in the WebAssembly module exports.

## Import Support

Goiaba supports importing functions from standard library packages. Currently supported packages include:

- `fmt`: Basic printing functions
- `math`: Mathematical functions
- `strings`: String manipulation functions

### Example

```go
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
```

When you compile this code, Goiaba will generate WASM imports for the stdlib functions used. The resulting WASM module can be instantiated with appropriate import objects that provide the implementations of these functions.

### Supported Functions

- `fmt.Println(string)` - Prints a string
- `math.Sqrt(float64) float64` - Square root function
- `strings.Len(string) int` - String length
- `strings.Join([]string, string) string` - Join strings with separator

Note: The current implementation provides basic support for these functions. Full Go standard library compatibility is planned for future versions.

## Development Status

### Completed Features

- [x] Go source code parsing to Abstract Syntax Tree (AST)
- [x] Translation of Go constructs to WebAssembly representations
- [x] WebAssembly bytecode generation
- [x] Function definitions with parameter and return types
- [x] Variable declarations and assignments
- [x] Control flow statements (if/else, for loops)
- [x] Exportable WASM functions
- [x] Arithmetic operations (+, -, *, /, %)
- [x] Comparison operations (<, >, <=, >=, ==, !=)
- [x] Bitwise operations (&, |, ^, <<, >>)
- [x] Logical operations (&&, ||, !)
- [x] Increment/decrement operators (++, --)
- [x] Recursive function calls
- [x] Struct types with field access and assignment
- [x] Command-line interface
- [x] Unary operators (negation, logical NOT)
- [x] Arrays and slices
- [x] String literals and operations
- [x] Switch statements

### In Development

- [ ] Pointer dereferencing and operations
- [ ] Methods on types
- [ ] Interfaces
- [ ] Multiple return values
- [ ] Defer statements
- [ ] Panic and recover
- [x] Package imports
- [ ] Standard library functions
- [ ] Floating-point operations
- [ ] Memory management optimizations

### Future Plans

- [ ] Goroutines and channels
- [ ] Complete standard library support
- [ ] Source maps for debugging
- [ ] Optimization passes for generated WASM
- [ ] JavaScript bindings generation (wasm-bindgen)
- [ ] Rust code generation
- [ ] Zig code generation
- [ ] LLVM-IR target compilation

## Architecture

Goiaba consists of several key components:

1. **Parser**: Lexical analysis and syntax parsing of Go source code
2. **AST**: Internal representation of Go program structure
3. **Translator**: Conversion from Go AST to WebAssembly IR
4. **Compiler**: Generation of WebAssembly bytecode
5. **CLI**: Command-line interface for user interaction

## Performance Considerations

The generated WebAssembly code prioritizes correctness over optimization. Future versions will include:

- Dead code elimination
- Constant folding
- Register allocation improvements
- Memory access optimization
- Function inlining for small functions

## Contributing

Contributions are welcome. Please ensure linting and tests pass before submitting pull requests:

```bash
task lint
task test
```

## Limitations

Current limitations of the compiler, yet to be added:

- No garbage collection (manual memory management)
- Limited standard library support
- No concurrency primitives (goroutines, channels)
- Single file compilation only
- No optimizer passes

## License

BSD-3-Clause

Copyright (c) 2024-present Raphael Amorim

## Acknowledgments

This project builds upon concepts from the Go language specification and WebAssembly standards. Parser implementation is adapted from the Goscript project.

## References

- https://go.dev/blog/ismmkeynote
