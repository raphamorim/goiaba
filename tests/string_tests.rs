// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

#[cfg(test)]
mod tests {
    use goiaba::wasm::compiler::compile_str;
    use wasmtime::{Engine, Instance, Module, Store};

    #[test]
    fn test_string_literal_compilation() {
        let go_source = r#"
            package main
            
            //export has_string
            func has_string() int {
                s := "hello"
                if s == "hello" {
                    return 1
                }
                return 0
            }
        "#;

        // Just verify it compiles without errors
        let result = compile_str(go_source);
        assert!(result.is_ok(), "Failed to compile: {:?}", result.err());
    }

    #[test]
    fn test_string_length() {
        let go_source = r#"
            package main
            
            //export string_len
            func string_len() int {
                s := "hello"
                return len(s)
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "string_len")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 5); // "hello" has 5 characters
    }

    #[test]
    fn test_empty_string_length() {
        let go_source = r#"
            package main
            
            //export empty_len
            func empty_len() int {
                s := ""
                return len(s)
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "empty_len")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 0);
    }

    #[test]
    fn test_multiple_string_lengths() {
        let go_source = r#"
            package main
            
            //export compare_lengths
            func compare_lengths() int {
                s1 := "hi"
                s2 := "world"
                return len(s1) + len(s2)
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "compare_lengths")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 7); // "hi" (2) + "world" (5)
    }

    #[test]
    fn test_string_length_conditional() {
        let go_source = r#"
            package main
            
            //export is_long
            func is_long() int {
                s := "hello world"
                if len(s) > 10 {
                    return 1
                }
                return 0
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "is_long")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 1);
    }

    #[test]
    fn test_string_special_characters() {
        let go_source = r#"
            package main
            
            //export special_chars_len
            func special_chars_len() int {
                s := "a b\tc"
                return len(s)
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "special_chars_len")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 5); // "a b\tc" = 5 characters
    }

    #[test]
    fn test_string_numeric_content() {
        let go_source = r#"
            package main
            
            //export numeric_string_len
            func numeric_string_len() int {
                s := "12345"
                return len(s)
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "numeric_string_len")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 5);
    }

    #[test]
    fn test_multiple_string_variables() {
        let go_source = r#"
            package main
            
            //export count_chars
            func count_chars() int {
                s1 := "abc"
                s2 := "defgh"
                s3 := "ij"
                total := len(s1) + len(s2) + len(s3)
                return total
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "count_chars")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 10); // 3 + 5 + 2
    }

    #[test]
    fn test_string_in_loop() {
        let go_source = r#"
            package main
            
            //export repeat_length
            func repeat_length() int {
                s := "test"
                total := 0
                for i := 0; i < 3; i++ {
                    total = total + len(s)
                }
                return total
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "repeat_length")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 12); // "test" length (4) * 3 iterations
    }

    #[test]
    fn test_string_length_comparison() {
        let go_source = r#"
            package main
            
            //export longer_string
            func longer_string() int {
                s1 := "short"
                s2 := "much longer string"
                
                if len(s1) > len(s2) {
                    return 1
                } else if len(s1) < len(s2) {
                    return 2
                } else {
                    return 3
                }
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "longer_string")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 2); // s2 is longer
    }

    #[test]
    fn test_string_with_calculation() {
        let go_source = r#"
            package main
            
            //export calculate_with_len
            func calculate_with_len() int {
                s := "golang"
                base := 10
                multiplier := len(s)
                return base * multiplier
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "calculate_with_len")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 60); // 10 * 6
    }
}
