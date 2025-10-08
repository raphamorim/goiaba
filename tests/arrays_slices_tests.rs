// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

#[cfg(test)]
mod tests {
    use goiaba::wasm::compiler::compile_str;
    use wasmtime::{Engine, Instance, Module, Store};

    #[test]
    fn test_array_literal_creation() {
        let go_source = r#"
            package main
            
            //export get_first
            func get_first() int {
                arr := []int{10, 20, 30}
                return arr[0]
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "get_first")
            .expect("Failed to get 'get_first' function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 10);
    }

    #[test]
    fn test_array_index_access() {
        let go_source = r#"
            package main
            
            //export get_element
            func get_element(index int) int {
                arr := []int{5, 10, 15, 20, 25}
                return arr[index]
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<i32, i32>(&mut store, "get_element")
            .expect("Failed to get function");

        assert_eq!(func.call(&mut store, 0).unwrap(), 5);
        assert_eq!(func.call(&mut store, 2).unwrap(), 15);
        assert_eq!(func.call(&mut store, 4).unwrap(), 25);
    }

    #[test]
    fn test_array_index_assignment() {
        let go_source = r#"
            package main
            
            //export set_and_get
            func set_and_get() int {
                arr := []int{1, 2, 3}
                arr[1] = 99
                return arr[1]
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "set_and_get")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 99);
    }

    #[test]
    fn test_array_sum() {
        let go_source = r#"
            package main
            
            //export sum_array
            func sum_array() int {
                arr := []int{10, 20, 30, 40}
                sum := 0
                for i := 0; i < 4; i++ {
                    sum = sum + arr[i]
                }
                return sum
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "sum_array")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 100); // 10 + 20 + 30 + 40
    }

    #[test]
    fn test_array_modification_loop() {
        let go_source = r#"
            package main
            
            //export double_elements
            func double_elements() int {
                arr := []int{1, 2, 3, 4, 5}
                for i := 0; i < 5; i++ {
                    arr[i] = arr[i] * 2
                }
                return arr[2]
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "double_elements")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 6); // 3 * 2
    }

    #[test]
    fn test_array_find_max() {
        let go_source = r#"
            package main
            
            //export find_max
            func find_max() int {
                arr := []int{3, 7, 2, 9, 1}
                max := arr[0]
                for i := 1; i < 5; i++ {
                    if arr[i] > max {
                        max = arr[i]
                    }
                }
                return max
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "find_max")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 9);
    }

    #[test]
    fn test_array_swap_elements() {
        let go_source = r#"
            package main
            
            //export swap_and_return
            func swap_and_return() int {
                arr := []int{100, 200}
                temp := arr[0]
                arr[0] = arr[1]
                arr[1] = temp
                return arr[0]
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "swap_and_return")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 200);
    }

    #[test]
    fn test_array_count_even() {
        let go_source = r#"
            package main
            
            //export count_even
            func count_even() int {
                arr := []int{1, 2, 3, 4, 5, 6}
                count := 0
                for i := 0; i < 6; i++ {
                    if arr[i] % 2 == 0 {
                        count++
                    }
                }
                return count
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "count_even")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 3); // 2, 4, 6
    }

    #[test]
    fn test_array_reverse() {
        let go_source = r#"
            package main
            
            //export reverse_array
            func reverse_array() int {
                arr := []int{1, 2, 3, 4, 5}
                left := 0
                right := 4
                
                for left < right {
                    temp := arr[left]
                    arr[left] = arr[right]
                    arr[right] = temp
                    left++
                    right--
                }
                
                return arr[0]
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "reverse_array")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 5); // First element after reversal
    }

    #[test]
    fn test_array_linear_search() {
        let go_source = r#"
            package main
            
            //export search
            func search(target int) int {
                arr := []int{10, 25, 30, 45, 50}
                for i := 0; i < 5; i++ {
                    if arr[i] == target {
                        return i
                    }
                }
                return -1
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<i32, i32>(&mut store, "search")
            .expect("Failed to get function");

        assert_eq!(func.call(&mut store, 30).unwrap(), 2);
        assert_eq!(func.call(&mut store, 50).unwrap(), 4);
        assert_eq!(func.call(&mut store, 100).unwrap(), -1);
    }

    #[test]
    fn test_nested_array_operations() {
        let go_source = r#"
            package main
            
            //export complex_operation
            func complex_operation() int {
                arr1 := []int{1, 2, 3}
                arr2 := []int{4, 5, 6}
                
                result := 0
                for i := 0; i < 3; i++ {
                    result = result + (arr1[i] * arr2[i])
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
            .get_typed_func::<(), i32>(&mut store, "complex_operation")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 32); // 1*4 + 2*5 + 3*6 = 4 + 10 + 18
    }

    #[test]
    fn test_array_accumulator() {
        let go_source = r#"
            package main
            
            //export accumulate
            func accumulate() int {
                arr := []int{5, 10, 15}
                arr[0] = arr[0] + arr[1]
                arr[1] = arr[0] + arr[2]
                return arr[1]
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "accumulate")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 30); // (5+10) + 15 = 30
    }

    #[test]
    fn test_array_conditional_update() {
        let go_source = r#"
            package main
            
            //export conditional_update
            func conditional_update() int {
                arr := []int{1, 5, 10, 15, 20}
                for i := 0; i < 5; i++ {
                    if arr[i] >= 10 {
                        arr[i] = 0
                    }
                }
                sum := 0
                for i := 0; i < 5; i++ {
                    sum = sum + arr[i]
                }
                return sum
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile Go to WASM");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load WASM module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &module, &[]).expect("Failed to instantiate module");

        let func = instance
            .get_typed_func::<(), i32>(&mut store, "conditional_update")
            .expect("Failed to get function");

        let result = func.call(&mut store, ()).expect("Failed to call function");
        assert_eq!(result, 6); // 1 + 5 + 0 + 0 + 0
    }
}
