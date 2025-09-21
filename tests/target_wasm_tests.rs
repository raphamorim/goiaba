// Copyright 2025-present Raphael Amorim. All rights reserved.
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

    #[test]
    fn test_wasm_execution_of_unary_operations() {
        let go_source = r#"
            package main
            
            //export negate
            func negate(x int) int {
                return -x
            }
            
            //export logical_not
            func logical_not(x int) int {
                return !x
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

        // Get the exported functions
        let negate_func = instance
            .get_typed_func::<i32, i32>(&mut store, "negate")
            .expect("Failed to get 'negate' function");

        let not_func = instance
            .get_typed_func::<i32, i32>(&mut store, "logical_not")
            .expect("Failed to get 'logical_not' function");

        // Test negation
        let result1 = negate_func
            .call(&mut store, 5)
            .expect("Failed to call 'negate' function");
        assert_eq!(result1, -5);

        // Test logical not
        let result2 = not_func
            .call(&mut store, 0)
            .expect("Failed to call 'logical_not' function");
        assert_eq!(result2, 1);

        let result3 = not_func
            .call(&mut store, 1)
            .expect("Failed to call 'logical_not' function");
        assert_eq!(result3, 0);
    }

    #[test]
    fn test_wasm_execution_of_loop_simulation() {
        let go_source = r#"
            package main
            
            //export sum_to_n
            func sum_to_n(n int) int {
                sum := 0
                i := 0
                // Simulate a loop with a condition
                for i < n {
                    sum = sum + i
                    i++
                }
                return sum
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
        let sum_func = instance
            .get_typed_func::<i32, i32>(&mut store, "sum_to_n")
            .expect("Failed to get 'sum_to_n' function");

        // Test sum calculation (0+1+2+3+4 = 10)
        let result = sum_func
            .call(&mut store, 5)
            .expect("Failed to call 'sum_to_n' function");
        assert_eq!(result, 10);
    }

    #[test]
    fn test_wasm_execution_of_nested_function_calls() {
        let go_source = r#"
            package main
            
            //export add
            func add(x int, y int) int {
                return x + y
            }
            
            //export multiply
            func multiply(x int, y int) int {
                return x * y
            }
            
            //export complex_calc
            func complex_calc(a int, b int, c int) int {
                temp1 := add(a, b)
                temp2 := multiply(temp1, c)
                return temp2
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
            .get_typed_func::<(i32, i32, i32), i32>(&mut store, "complex_calc")
            .expect("Failed to get 'complex_calc' function");

        // Test complex calculation: (2 + 3) * 4 = 20
        let result = calc_func
            .call(&mut store, (2, 3, 4))
            .expect("Failed to call 'complex_calc' function");
        assert_eq!(result, 20);
    }

    #[test]
    fn test_wasm_execution_of_bitwise_operations() {
        let go_source = r#"
            package main
            
            //export bitwise_and
            func bitwise_and(a int, b int) int {
                return a & b
            }
            
            //export bitwise_or
            func bitwise_or(a int, b int) int {
                return a | b
            }
            
            //export bitwise_xor
            func bitwise_xor(a int, b int) int {
                return a ^ b
            }
            
            //export left_shift
            func left_shift(a int, b int) int {
                return a << b
            }
            
            //export right_shift
            func right_shift(a int, b int) int {
                return a >> b
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        // Test bitwise AND
        let and_func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "bitwise_and")
            .unwrap();
        assert_eq!(and_func.call(&mut store, (12, 10)).unwrap(), 8); // 1100 & 1010 = 1000

        // Test bitwise OR
        let or_func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "bitwise_or")
            .unwrap();
        assert_eq!(or_func.call(&mut store, (12, 10)).unwrap(), 14); // 1100 | 1010 = 1110

        // Test bitwise XOR
        let xor_func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "bitwise_xor")
            .unwrap();
        assert_eq!(xor_func.call(&mut store, (12, 10)).unwrap(), 6); // 1100 ^ 1010 = 0110

        // Test left shift
        let lshift_func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "left_shift")
            .unwrap();
        assert_eq!(lshift_func.call(&mut store, (5, 2)).unwrap(), 20); // 5 << 2 = 20

        // Test right shift
        let rshift_func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "right_shift")
            .unwrap();
        assert_eq!(rshift_func.call(&mut store, (20, 2)).unwrap(), 5); // 20 >> 2 = 5
    }

    #[test]
    fn test_wasm_execution_of_comparison_operations() {
        let go_source = r#"
            package main
            
            //export equals
            func equals(a int, b int) int {
                if a == b {
                    return 1
                }
                return 0
            }
            
            //export not_equals
            func not_equals(a int, b int) int {
                if a != b {
                    return 1
                }
                return 0
            }
            
            //export less_than
            func less_than(a int, b int) int {
                if a < b {
                    return 1
                }
                return 0
            }
            
            //export greater_equal
            func greater_equal(a int, b int) int {
                if a >= b {
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

        let eq_func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "equals")
            .unwrap();
        let neq_func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "not_equals")
            .unwrap();
        let lt_func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "less_than")
            .unwrap();
        let ge_func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "greater_equal")
            .unwrap();

        // Test equality
        assert_eq!(eq_func.call(&mut store, (5, 5)).unwrap(), 1);
        assert_eq!(eq_func.call(&mut store, (5, 3)).unwrap(), 0);

        // Test inequality
        assert_eq!(neq_func.call(&mut store, (5, 3)).unwrap(), 1);
        assert_eq!(neq_func.call(&mut store, (5, 5)).unwrap(), 0);

        // Test less than
        assert_eq!(lt_func.call(&mut store, (3, 5)).unwrap(), 1);
        assert_eq!(lt_func.call(&mut store, (5, 3)).unwrap(), 0);

        // Test greater than or equal
        assert_eq!(ge_func.call(&mut store, (5, 3)).unwrap(), 1);
        assert_eq!(ge_func.call(&mut store, (5, 5)).unwrap(), 1);
        assert_eq!(ge_func.call(&mut store, (3, 5)).unwrap(), 0);
    }

    #[test]
    fn test_wasm_execution_of_variable_assignments() {
        let go_source = r#"
            package main
            
            //export complex_assignment
            func complex_assignment(x int) int {
                a := x
                b := a * 2
                c := b + 1
                a = c - 3
                return a + b + c
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<i32, i32>(&mut store, "complex_assignment")
            .unwrap();

        // With x=5: a=5, b=10, c=11, then a=8, result=8+10+11=29
        assert_eq!(func.call(&mut store, 5).unwrap(), 29);
    }

    #[test]
    fn test_wasm_execution_of_modulo_operations() {
        let go_source = r#"
            package main
            
            //export modulo
            func modulo(a int, b int) int {
                return a % b
            }
            
            //export is_even
            func is_even(n int) int {
                if n % 2 == 0 {
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

        let mod_func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "modulo")
            .unwrap();
        let even_func = instance
            .get_typed_func::<i32, i32>(&mut store, "is_even")
            .unwrap();

        // Test modulo operation
        assert_eq!(mod_func.call(&mut store, (17, 5)).unwrap(), 2);
        assert_eq!(mod_func.call(&mut store, (20, 4)).unwrap(), 0);

        // Test even number detection
        assert_eq!(even_func.call(&mut store, 4).unwrap(), 1);
        assert_eq!(even_func.call(&mut store, 7).unwrap(), 0);
    }

    #[test]
    fn test_wasm_execution_of_nested_conditionals() {
        let go_source = r#"
            package main
            
            //export grade_classifier
            func grade_classifier(score int) int {
                if score >= 90 {
                    return 4 // A
                } else if score >= 80 {
                    return 3 // B
                } else if score >= 70 {
                    return 2 // C
                } else if score >= 60 {
                    return 1 // D
                } else {
                    return 0 // F
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
            .get_typed_func::<i32, i32>(&mut store, "grade_classifier")
            .unwrap();

        assert_eq!(func.call(&mut store, 95).unwrap(), 4); // A
        assert_eq!(func.call(&mut store, 85).unwrap(), 3); // B
        assert_eq!(func.call(&mut store, 75).unwrap(), 2); // C
        assert_eq!(func.call(&mut store, 65).unwrap(), 1); // D
        assert_eq!(func.call(&mut store, 45).unwrap(), 0); // F
    }

    #[test]
    fn test_wasm_execution_of_power_function() {
        let go_source = r#"
            package main
            
            //export power
            func power(base int, exp int) int {
                if exp == 0 {
                    return 1
                }
                if exp == 1 {
                    return base
                }
                if exp < 0 {
                    return 0 // Simple handling for negative exponents
                }
                
                result := 1
                i := 0
                for i < exp {
                    result = result * base
                    i++
                }
                return result
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "power")
            .unwrap();

        assert_eq!(func.call(&mut store, (2, 0)).unwrap(), 1); // 2^0 = 1
        assert_eq!(func.call(&mut store, (2, 1)).unwrap(), 2); // 2^1 = 2
        assert_eq!(func.call(&mut store, (2, 3)).unwrap(), 8); // 2^3 = 8
        assert_eq!(func.call(&mut store, (3, 4)).unwrap(), 81); // 3^4 = 81
        assert_eq!(func.call(&mut store, (5, 2)).unwrap(), 25); // 5^2 = 25
    }

    #[test]
    fn test_wasm_execution_of_factorial() {
        let go_source = r#"
            package main
            
            //export factorial
            func factorial(n int) int {
                if n <= 1 {
                    return 1
                }
                return n * factorial(n-1)
            }
            
            //export factorial_iterative
            func factorial_iterative(n int) int {
                if n <= 1 {
                    return 1
                }
                result := 1
                i := 2
                for i <= n {
                    result = result * i
                    i++
                }
                return result
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let recursive_func = instance
            .get_typed_func::<i32, i32>(&mut store, "factorial")
            .unwrap();
        let iterative_func = instance
            .get_typed_func::<i32, i32>(&mut store, "factorial_iterative")
            .unwrap();

        // Test recursive factorial
        assert_eq!(recursive_func.call(&mut store, 0).unwrap(), 1); // 0! = 1
        assert_eq!(recursive_func.call(&mut store, 1).unwrap(), 1); // 1! = 1
        assert_eq!(recursive_func.call(&mut store, 5).unwrap(), 120); // 5! = 120

        // Test iterative factorial
        assert_eq!(iterative_func.call(&mut store, 0).unwrap(), 1);
        assert_eq!(iterative_func.call(&mut store, 1).unwrap(), 1);
        assert_eq!(iterative_func.call(&mut store, 5).unwrap(), 120);
    }

    #[test]
    fn test_wasm_execution_of_gcd_algorithm() {
        let go_source = r#"
            package main
            
            //export gcd
            func gcd(a int, b int) int {
                for b != 0 {
                    temp := b
                    b = a % b
                    a = temp
                }
                return a
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "gcd")
            .unwrap();

        assert_eq!(func.call(&mut store, (48, 18)).unwrap(), 6);
        assert_eq!(func.call(&mut store, (17, 13)).unwrap(), 1);
        assert_eq!(func.call(&mut store, (100, 25)).unwrap(), 25);
    }

    #[test]
    fn test_wasm_execution_of_prime_checker() {
        let go_source = r#"
            package main
            
            //export is_prime
            func is_prime(n int) int {
                if n <= 1 {
                    return 0
                }
                if n <= 3 {
                    return 1
                }
                if n % 2 == 0 || n % 3 == 0 {
                    return 0
                }
                
                i := 5
                for i * i <= n {
                    if n % i == 0 || n % (i + 2) == 0 {
                        return 0
                    }
                    i = i + 6
                }
                return 1
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<i32, i32>(&mut store, "is_prime")
            .unwrap();

        assert_eq!(func.call(&mut store, 1).unwrap(), 0); // Not prime
        assert_eq!(func.call(&mut store, 2).unwrap(), 1); // Prime
        assert_eq!(func.call(&mut store, 3).unwrap(), 1); // Prime
        assert_eq!(func.call(&mut store, 4).unwrap(), 0); // Not prime
        assert_eq!(func.call(&mut store, 17).unwrap(), 1); // Prime
        assert_eq!(func.call(&mut store, 25).unwrap(), 0); // Not prime
        assert_eq!(func.call(&mut store, 29).unwrap(), 1); // Prime
    }

    #[test]
    fn test_wasm_execution_with_zero_and_negative_numbers() {
        let go_source = r#"
            package main
            
            //export abs_value
            func abs_value(x int) int {
                if x < 0 {
                    return -x
                }
                return x
            }
            
            //export sign
            func sign(x int) int {
                if x > 0 {
                    return 1
                } else if x < 0 {
                    return -1
                } else {
                    return 0
                }
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let abs_func = instance
            .get_typed_func::<i32, i32>(&mut store, "abs_value")
            .unwrap();
        let sign_func = instance
            .get_typed_func::<i32, i32>(&mut store, "sign")
            .unwrap();

        // Test absolute value
        assert_eq!(abs_func.call(&mut store, 5).unwrap(), 5);
        assert_eq!(abs_func.call(&mut store, -5).unwrap(), 5);
        assert_eq!(abs_func.call(&mut store, 0).unwrap(), 0);

        // Test sign function
        assert_eq!(sign_func.call(&mut store, 42).unwrap(), 1);
        assert_eq!(sign_func.call(&mut store, -42).unwrap(), -1);
        assert_eq!(sign_func.call(&mut store, 0).unwrap(), 0);
    }

    #[test]
    fn test_wasm_execution_of_multiple_return_paths() {
        let go_source = r#"
            package main
            
            //export classify_number
            func classify_number(n int) int {
                // Returns: 0=negative, 1=zero, 2=positive_small(<10), 3=positive_large(>=10)
                if n < 0 {
                    return 0
                }
                if n == 0 {
                    return 1
                }
                if n < 10 {
                    return 2
                }
                return 3
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<i32, i32>(&mut store, "classify_number")
            .unwrap();

        assert_eq!(func.call(&mut store, -5).unwrap(), 0); // Negative
        assert_eq!(func.call(&mut store, 0).unwrap(), 1); // Zero
        assert_eq!(func.call(&mut store, 5).unwrap(), 2); // Positive small
        assert_eq!(func.call(&mut store, 15).unwrap(), 3); // Positive large
    }

    #[test]
    fn test_wasm_execution_with_structs() {
        let go_source = r#"
            package main
            
            type Point struct {
                x int
                y int
            }
            
            type Rectangle struct {
                width  int
                height int
            }
            
            //export create_point
            func create_point(x int, y int) int {
                p := Point{x: x, y: y}
                return p.x + p.y
            }
            
            //export point_distance_squared
            func point_distance_squared(x1 int, y1 int, x2 int, y2 int) int {
                p1 := Point{x: x1, y: y1}
                p2 := Point{x: x2, y: y2}
                
                dx := p1.x - p2.x
                dy := p1.y - p2.y
                
                return dx*dx + dy*dy
            }
            
            //export rectangle_area
            func rectangle_area(width int, height int) int {
                rect := Rectangle{
                    width:  width,
                    height: height,
                }
                return rect.width * rect.height
            }
            
            //export rectangle_perimeter
            func rectangle_perimeter(width int, height int) int {
                rect := Rectangle{width: width, height: height}
                return 2 * (rect.width + rect.height)
            }
            
            //export update_point_coordinates
            func update_point_coordinates(x int, y int, deltaX int, deltaY int) int {
                point := Point{x: x, y: y}
                
                // Update coordinates
                point.x = point.x + deltaX
                point.y = point.y + deltaY
                
                // Return sum of new coordinates
                return point.x + point.y
            }
            
            //export compare_rectangles
            func compare_rectangles(w1 int, h1 int, w2 int, h2 int) int {
                rect1 := Rectangle{width: w1, height: h1}
                rect2 := Rectangle{width: w2, height: h2}
                
                area1 := rect1.width * rect1.height
                area2 := rect2.width * rect2.height
                
                if area1 > area2 {
                    return 1  // rect1 is larger
                } else if area1 < area2 {
                    return -1 // rect2 is larger
                } else {
                    return 0  // equal areas
                }
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        // Test basic struct creation and field access
        let create_point_func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "create_point")
            .expect("Failed to get 'create_point' function");
        assert_eq!(create_point_func.call(&mut store, (3, 4)).unwrap(), 7);

        // Test struct operations with calculations
        let distance_func = instance
            .get_typed_func::<(i32, i32, i32, i32), i32>(&mut store, "point_distance_squared")
            .expect("Failed to get 'point_distance_squared' function");
        // Distance squared between (0,0) and (3,4) should be 3²+4² = 25
        assert_eq!(distance_func.call(&mut store, (0, 0, 3, 4)).unwrap(), 25);

        // Test rectangle area calculation
        let area_func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "rectangle_area")
            .expect("Failed to get 'rectangle_area' function");
        assert_eq!(area_func.call(&mut store, (5, 3)).unwrap(), 15);

        // Test rectangle perimeter calculation
        let perimeter_func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "rectangle_perimeter")
            .expect("Failed to get 'rectangle_perimeter' function");
        assert_eq!(perimeter_func.call(&mut store, (5, 3)).unwrap(), 16); // 2*(5+3) = 16

        // Test struct field updates
        let update_func = instance
            .get_typed_func::<(i32, i32, i32, i32), i32>(&mut store, "update_point_coordinates")
            .expect("Failed to get 'update_point_coordinates' function");
        // Start at (2,3), add (1,4) -> (3,7) -> sum = 10
        assert_eq!(update_func.call(&mut store, (2, 3, 1, 4)).unwrap(), 10);

        // Test struct comparison logic
        let compare_func = instance
            .get_typed_func::<(i32, i32, i32, i32), i32>(&mut store, "compare_rectangles")
            .expect("Failed to get 'compare_rectangles' function");

        // Rectangle 1: 4x3=12, Rectangle 2: 3x3=9 -> rect1 is larger
        assert_eq!(compare_func.call(&mut store, (4, 3, 3, 3)).unwrap(), 1);

        // Rectangle 1: 2x3=6, Rectangle 2: 3x3=9 -> rect2 is larger
        assert_eq!(compare_func.call(&mut store, (2, 3, 3, 3)).unwrap(), -1);

        // Rectangle 1: 3x3=9, Rectangle 2: 3x3=9 -> equal areas
        assert_eq!(compare_func.call(&mut store, (3, 3, 3, 3)).unwrap(), 0);
    }
}
