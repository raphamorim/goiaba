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

## License

GPL-3.0