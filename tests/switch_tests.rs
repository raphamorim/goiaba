// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

#[cfg(test)]
mod tests {
    use goiaba::wasm::compiler::compile_str;
    use wasmtime::{Engine, Instance, Module, Store};

    #[test]
    fn test_simple_switch() {
        let go_source = r#"
            package main
            
            //export classify
            func classify(x int) int {
                switch x {
                case 1:
                    return 10
                case 2:
                    return 20
                case 3:
                    return 30
                default:
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

        let func = instance
            .get_typed_func::<i32, i32>(&mut store, "classify")
            .expect("Failed to get function");

        assert_eq!(func.call(&mut store, 1).unwrap(), 10);
        assert_eq!(func.call(&mut store, 2).unwrap(), 20);
        assert_eq!(func.call(&mut store, 3).unwrap(), 30);
        assert_eq!(func.call(&mut store, 99).unwrap(), 0);
    }

    #[test]
    fn test_switch_with_default() {
        let go_source = r#"
            package main
            
            //export get_day_type
            func get_day_type(day int) int {
                switch day {
                case 1, 2, 3, 4, 5:
                    return 1  // Weekday
                case 6, 7:
                    return 2  // Weekend
                default:
                    return 0  // Invalid
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
            .get_typed_func::<i32, i32>(&mut store, "get_day_type")
            .expect("Failed to get function");

        assert_eq!(func.call(&mut store, 1).unwrap(), 1);
        assert_eq!(func.call(&mut store, 3).unwrap(), 1);
        assert_eq!(func.call(&mut store, 5).unwrap(), 1);
        assert_eq!(func.call(&mut store, 6).unwrap(), 2);
        assert_eq!(func.call(&mut store, 7).unwrap(), 2);
        assert_eq!(func.call(&mut store, 10).unwrap(), 0);
    }

    #[test]
    fn test_switch_calculation_in_case() {
        let go_source = r#"
            package main
            
            //export compute
            func compute(op int, a int, b int) int {
                switch op {
                case 1:
                    return a + b
                case 2:
                    return a - b
                case 3:
                    return a * b
                case 4:
                    return a / b
                default:
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

        let func = instance
            .get_typed_func::<(i32, i32, i32), i32>(&mut store, "compute")
            .expect("Failed to get function");

        assert_eq!(func.call(&mut store, (1, 10, 5)).unwrap(), 15); // add
        assert_eq!(func.call(&mut store, (2, 10, 5)).unwrap(), 5); // sub
        assert_eq!(func.call(&mut store, (3, 10, 5)).unwrap(), 50); // mul
        assert_eq!(func.call(&mut store, (4, 10, 5)).unwrap(), 2); // div
        assert_eq!(func.call(&mut store, (99, 10, 5)).unwrap(), 0); // default
    }

    #[test]
    fn test_switch_with_variables() {
        let go_source = r#"
            package main
            
            //export evaluate
            func evaluate(x int) int {
                result := 0
                switch x {
                case 1:
                    result = 100
                case 2:
                    result = 200
                case 3:
                    result = 300
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
            .get_typed_func::<i32, i32>(&mut store, "evaluate")
            .expect("Failed to get function");

        assert_eq!(func.call(&mut store, 1).unwrap(), 100);
        assert_eq!(func.call(&mut store, 2).unwrap(), 200);
        assert_eq!(func.call(&mut store, 3).unwrap(), 300);
        assert_eq!(func.call(&mut store, 99).unwrap(), 0); // No default case
    }

    #[test]
    fn test_nested_switch() {
        let go_source = r#"
            package main
            
            //export categorize
            func categorize(x int, y int) int {
                switch x {
                case 1:
                    switch y {
                    case 1:
                        return 11
                    case 2:
                        return 12
                    default:
                        return 10
                    }
                case 2:
                    return 20
                default:
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

        let func = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "categorize")
            .expect("Failed to get function");

        assert_eq!(func.call(&mut store, (1, 1)).unwrap(), 11);
        assert_eq!(func.call(&mut store, (1, 2)).unwrap(), 12);
        assert_eq!(func.call(&mut store, (1, 3)).unwrap(), 10);
        assert_eq!(func.call(&mut store, (2, 1)).unwrap(), 20);
        assert_eq!(func.call(&mut store, (3, 1)).unwrap(), 0);
    }

    #[test]
    fn test_switch_only_default() {
        let go_source = r#"
            package main
            
            //export always_42
            func always_42(x int) int {
                switch x {
                default:
                    return 42
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
            .get_typed_func::<i32, i32>(&mut store, "always_42")
            .expect("Failed to get function");

        assert_eq!(func.call(&mut store, 1).unwrap(), 42);
        assert_eq!(func.call(&mut store, 100).unwrap(), 42);
        assert_eq!(func.call(&mut store, -5).unwrap(), 42);
    }
}
