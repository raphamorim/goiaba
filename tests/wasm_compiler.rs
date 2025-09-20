// Copyright 2025 Raphael Amorim. All rights reserved.
// Use of this source code is governed by a GPL-3.0
// license that can be found in the LICENSE file.

#[cfg(test)]
mod tests {
    use goiaba::wasm::compiler::compile_str;
    use wasmtime::{Engine, Instance, Module, Store};

    #[test]
    fn test_compile_str_with_simple_function() {
        let go_source = r#"
            package main
            
            //export add
            func add(x int, y int) int {
                return x + y
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");
        assert!(!wasm_bytes.is_empty());

        // Verify it's a valid WASM module by checking the magic number
        assert_eq!(&wasm_bytes[0..4], &[0x00, 0x61, 0x73, 0x6d]);
    }

    #[test]
    fn test_wasm_execution_of_compiled_function() {
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

    #[test]
    fn test_wasm_execution_of_fibonacci() {
        let go_source = r#"
            package main
            
            //export fibonacci
            func fibonacci(n int) int {
                if n <= 1 {
                    return n
                }
                return fibonacci(n-1) + fibonacci(n-2)
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
        let fib_func = instance
            .get_typed_func::<i32, i32>(&mut store, "fibonacci")
            .expect("Failed to get 'fibonacci' function");

        // Call the function
        let result = fib_func
            .call(&mut store, 10)
            .expect("Failed to call 'fibonacci' function");

        // Verify the result (10th Fibonacci number is 55)
        assert_eq!(result, 55);
    }

    #[test]
    fn test_wasm_execution_of_complex_arithmetic() {
        let go_source = r#"
            package main
            
            //export calculate
            func calculate(a int, b int) int {
                result := a * b + a - b
                result++
                return result
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
        let calc_func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "calculate")
            .expect("Failed to get 'calculate' function");

        // Call the function
        let result = calc_func
            .call(&mut store, (5, 3))
            .expect("Failed to call 'calculate' function");

        // Verify the result: 5 * 3 + 5 - 3 = 15 + 5 - 3 = 17, then increment to 18
        assert_eq!(result, 18);
    }

    #[test]
    fn test_wasm_execution_of_conditional_logic() {
        let go_source = r#"
            package main
            
            //export max
            func max(a int, b int) int {
                result := 0
                if a >= b {
                    result = a
                } else {
                    result = b
                }
                return result
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
        let max_func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "max")
            .expect("Failed to get 'max' function");

        // Test case 1: a > b
        let result1 = max_func
            .call(&mut store, (10, 5))
            .expect("Failed to call 'max' function");
        assert_eq!(result1, 10);

        // Test case 2: b > a
        let result2 = max_func
            .call(&mut store, (3, 8))
            .expect("Failed to call 'max' function");
        assert_eq!(result2, 8);

        // Test case 3: a == b
        let result3 = max_func
            .call(&mut store, (7, 7))
            .expect("Failed to call 'max' function");
        assert_eq!(result3, 7);
    }
}
