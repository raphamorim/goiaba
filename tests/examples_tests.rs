// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

#[cfg(test)]
mod tests {
    use goiaba::wasm::compiler::compile_str;
    use std::fs;
    use wasmtime::{Engine, Instance, Module, Store};

    /// Helper function to compile and validate a Go source file
    fn compile_and_validate(filename: &str) -> Vec<u8> {
        let path = format!("go_examples/{}", filename);
        let source =
            fs::read_to_string(&path).unwrap_or_else(|_| panic!("Failed to read {}", path));

        let wasm_bytes = compile_str(&source)
            .unwrap_or_else(|e| panic!("Failed to compile {}: {}", filename, e));

        // Validate the WASM
        let engine = Engine::default();
        Module::from_binary(&engine, &wasm_bytes)
            .unwrap_or_else(|e| panic!("Invalid WASM generated from {}: {}", filename, e));

        wasm_bytes
    }

    /// Helper to get a typed function from WASM instance
    fn get_func<Params, Results>(
        instance: &Instance,
        store: &mut Store<()>,
        name: &str,
    ) -> wasmtime::TypedFunc<Params, Results>
    where
        Params: wasmtime::WasmParams,
        Results: wasmtime::WasmResults,
    {
        instance
            .get_typed_func::<Params, Results>(store, name)
            .unwrap_or_else(|e| panic!("Failed to get function '{}': {}", name, e))
    }

    #[test]
    fn test_calculator_example() {
        let wasm_bytes = compile_and_validate("calculator.go");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).unwrap();
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).unwrap();

        // Test basic operations
        let add = get_func::<(i32, i32), i32>(&instance, &mut store, "add");
        assert_eq!(add.call(&mut store, (5, 3)).unwrap(), 8);
        assert_eq!(add.call(&mut store, (100, 200)).unwrap(), 300);

        let multiply = get_func::<(i32, i32), i32>(&instance, &mut store, "multiply");
        assert_eq!(multiply.call(&mut store, (4, 7)).unwrap(), 28);
        assert_eq!(multiply.call(&mut store, (10, 10)).unwrap(), 100);

        // Test recursive functions
        let factorial = get_func::<i32, i32>(&instance, &mut store, "factorial");
        assert_eq!(factorial.call(&mut store, 5).unwrap(), 120);
        assert_eq!(factorial.call(&mut store, 0).unwrap(), 1);
        assert_eq!(factorial.call(&mut store, 7).unwrap(), 5040);

        let fibonacci = get_func::<i32, i32>(&instance, &mut store, "fibonacci");
        assert_eq!(fibonacci.call(&mut store, 0).unwrap(), 0);
        assert_eq!(fibonacci.call(&mut store, 1).unwrap(), 1);
        assert_eq!(fibonacci.call(&mut store, 10).unwrap(), 55);

        // Test conditional logic
        let max = get_func::<(i32, i32), i32>(&instance, &mut store, "max");
        assert_eq!(max.call(&mut store, (10, 5)).unwrap(), 10);
        assert_eq!(max.call(&mut store, (3, 8)).unwrap(), 8);

        let abs = get_func::<i32, i32>(&instance, &mut store, "abs");
        assert_eq!(abs.call(&mut store, -5).unwrap(), 5);
        assert_eq!(abs.call(&mut store, 10).unwrap(), 10);

        let sign = get_func::<i32, i32>(&instance, &mut store, "sign");
        assert_eq!(sign.call(&mut store, 42).unwrap(), 1);
        assert_eq!(sign.call(&mut store, -42).unwrap(), -1);
        assert_eq!(sign.call(&mut store, 0).unwrap(), 0);

        // Test loops
        let sum_to_n = get_func::<i32, i32>(&instance, &mut store, "sum_to_n");
        assert_eq!(sum_to_n.call(&mut store, 10).unwrap(), 45);

        let power = get_func::<(i32, i32), i32>(&instance, &mut store, "power");
        assert_eq!(power.call(&mut store, (2, 8)).unwrap(), 256);
        assert_eq!(power.call(&mut store, (3, 4)).unwrap(), 81);

        // Test bitwise operations
        let bitwise_and = get_func::<(i32, i32), i32>(&instance, &mut store, "bitwise_and");
        assert_eq!(bitwise_and.call(&mut store, (12, 10)).unwrap(), 8);

        let bitwise_or = get_func::<(i32, i32), i32>(&instance, &mut store, "bitwise_or");
        assert_eq!(bitwise_or.call(&mut store, (12, 10)).unwrap(), 14);

        let left_shift = get_func::<(i32, i32), i32>(&instance, &mut store, "left_shift");
        assert_eq!(left_shift.call(&mut store, (1, 3)).unwrap(), 8);
    }

    #[test]
    fn test_arrays_example() {
        let wasm_bytes = compile_and_validate("arrays.go");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).unwrap();
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).unwrap();

        let array_sum = get_func::<(), i32>(&instance, &mut store, "array_sum");
        assert_eq!(array_sum.call(&mut store, ()).unwrap(), 150); // 10+20+30+40+50

        let find_max = get_func::<(), i32>(&instance, &mut store, "find_max");
        assert_eq!(find_max.call(&mut store, ()).unwrap(), 12);

        let find_min = get_func::<(), i32>(&instance, &mut store, "find_min");
        assert_eq!(find_min.call(&mut store, ()).unwrap(), 3);

        let reverse_array = get_func::<(), i32>(&instance, &mut store, "reverse_array");
        assert_eq!(reverse_array.call(&mut store, ()).unwrap(), 5);

        let linear_search = get_func::<i32, i32>(&instance, &mut store, "linear_search");
        assert_eq!(linear_search.call(&mut store, 30).unwrap(), 2);
        assert_eq!(linear_search.call(&mut store, 50).unwrap(), 4);
        assert_eq!(linear_search.call(&mut store, 99).unwrap(), -1);

        let count_even = get_func::<(), i32>(&instance, &mut store, "count_even");
        assert_eq!(count_even.call(&mut store, ()).unwrap(), 5);

        let array_product = get_func::<(), i32>(&instance, &mut store, "array_product");
        assert_eq!(array_product.call(&mut store, ()).unwrap(), 120);

        let count_positive = get_func::<(), i32>(&instance, &mut store, "count_positive");
        assert_eq!(count_positive.call(&mut store, ()).unwrap(), 3);

        let array_average = get_func::<(), i32>(&instance, &mut store, "array_average");
        assert_eq!(array_average.call(&mut store, ()).unwrap(), 30);
    }

    #[test]
    fn test_strings_example() {
        let wasm_bytes = compile_and_validate("strings.go");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).unwrap();
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).unwrap();

        let string_length = get_func::<(), i32>(&instance, &mut store, "string_length");
        assert_eq!(string_length.call(&mut store, ()).unwrap(), 13);

        let empty_string_check = get_func::<(), i32>(&instance, &mut store, "empty_string_check");
        assert_eq!(empty_string_check.call(&mut store, ()).unwrap(), 1);

        let compare_lengths = get_func::<(), i32>(&instance, &mut store, "compare_lengths");
        assert_eq!(compare_lengths.call(&mut store, ()).unwrap(), -1);

        let total_chars = get_func::<(), i32>(&instance, &mut store, "total_chars");
        assert_eq!(total_chars.call(&mut store, ()).unwrap(), 11);

        let is_long_string = get_func::<(), i32>(&instance, &mut store, "is_long_string");
        assert_eq!(is_long_string.call(&mut store, ()).unwrap(), 1);

        let validate_input = get_func::<(), i32>(&instance, &mut store, "validate_input");
        assert_eq!(validate_input.call(&mut store, ()).unwrap(), 1);

        let escape_sequences = get_func::<(), i32>(&instance, &mut store, "escape_sequences");
        assert_eq!(escape_sequences.call(&mut store, ()).unwrap(), 15);

        let string_with_numbers = get_func::<(), i32>(&instance, &mut store, "string_with_numbers");
        assert_eq!(string_with_numbers.call(&mut store, ()).unwrap(), 11);

        let max_string_length = get_func::<(), i32>(&instance, &mut store, "max_string_length");
        assert_eq!(max_string_length.call(&mut store, ()).unwrap(), 21);
    }

    #[test]
    fn test_structs_example() {
        let wasm_bytes = compile_and_validate("structs.go");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).unwrap();
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).unwrap();

        let create_point = get_func::<(i32, i32), i32>(&instance, &mut store, "create_point");
        assert_eq!(create_point.call(&mut store, (3, 4)).unwrap(), 7);

        let point_distance_squared =
            get_func::<(i32, i32, i32, i32), i32>(&instance, &mut store, "point_distance_squared");
        assert_eq!(
            point_distance_squared
                .call(&mut store, (0, 0, 3, 4))
                .unwrap(),
            25
        );

        let rectangle_area = get_func::<(i32, i32), i32>(&instance, &mut store, "rectangle_area");
        assert_eq!(rectangle_area.call(&mut store, (5, 10)).unwrap(), 50);

        let rectangle_perimeter =
            get_func::<(i32, i32), i32>(&instance, &mut store, "rectangle_perimeter");
        assert_eq!(rectangle_perimeter.call(&mut store, (5, 10)).unwrap(), 30);

        let circle_area_approx = get_func::<i32, i32>(&instance, &mut store, "circle_area_approx");
        assert_eq!(circle_area_approx.call(&mut store, 5).unwrap(), 75);

        let update_point =
            get_func::<(i32, i32, i32, i32), i32>(&instance, &mut store, "update_point");
        assert_eq!(update_point.call(&mut store, (10, 20, 5, -3)).unwrap(), 32);

        let compare_rectangles =
            get_func::<(i32, i32, i32, i32), i32>(&instance, &mut store, "compare_rectangles");
        assert_eq!(
            compare_rectangles.call(&mut store, (10, 5, 4, 6)).unwrap(),
            1
        );

        let point_quadrant = get_func::<(i32, i32), i32>(&instance, &mut store, "point_quadrant");
        assert_eq!(point_quadrant.call(&mut store, (5, 5)).unwrap(), 1);
        assert_eq!(point_quadrant.call(&mut store, (-5, 5)).unwrap(), 2);

        let person_is_adult = get_func::<i32, i32>(&instance, &mut store, "person_is_adult");
        assert_eq!(person_is_adult.call(&mut store, 18).unwrap(), 1);
        assert_eq!(person_is_adult.call(&mut store, 17).unwrap(), 0);
    }

    #[test]
    fn test_switch_example() {
        let wasm_bytes = compile_and_validate("switch.go");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).unwrap();
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).unwrap();

        let day_of_week = get_func::<i32, i32>(&instance, &mut store, "day_of_week");
        assert_eq!(day_of_week.call(&mut store, 1).unwrap(), 100);
        assert_eq!(day_of_week.call(&mut store, 7).unwrap(), 700);

        let is_weekday = get_func::<i32, i32>(&instance, &mut store, "is_weekday");
        assert_eq!(is_weekday.call(&mut store, 3).unwrap(), 1);
        assert_eq!(is_weekday.call(&mut store, 6).unwrap(), 0);

        let season_from_month = get_func::<i32, i32>(&instance, &mut store, "season_from_month");
        assert_eq!(season_from_month.call(&mut store, 1).unwrap(), 1);
        assert_eq!(season_from_month.call(&mut store, 7).unwrap(), 3);

        let calculator_switch =
            get_func::<(i32, i32, i32), i32>(&instance, &mut store, "calculator_switch");
        assert_eq!(calculator_switch.call(&mut store, (1, 10, 5)).unwrap(), 15);
        assert_eq!(calculator_switch.call(&mut store, (3, 10, 5)).unwrap(), 50);

        let http_status_category =
            get_func::<i32, i32>(&instance, &mut store, "http_status_category");
        assert_eq!(http_status_category.call(&mut store, 200).unwrap(), 1);
        assert_eq!(http_status_category.call(&mut store, 404).unwrap(), 2);

        let traffic_light = get_func::<i32, i32>(&instance, &mut store, "traffic_light");
        assert_eq!(traffic_light.call(&mut store, 1).unwrap(), 10);
        assert_eq!(traffic_light.call(&mut store, 3).unwrap(), 30);

        let month_days = get_func::<i32, i32>(&instance, &mut store, "month_days");
        assert_eq!(month_days.call(&mut store, 1).unwrap(), 31);
        assert_eq!(month_days.call(&mut store, 4).unwrap(), 30);
        assert_eq!(month_days.call(&mut store, 2).unwrap(), 28);

        let priority_level = get_func::<(i32, i32), i32>(&instance, &mut store, "priority_level");
        assert_eq!(priority_level.call(&mut store, (1, 1)).unwrap(), 4);
        assert_eq!(priority_level.call(&mut store, (2, 2)).unwrap(), 2);
    }

    #[test]
    fn test_algorithms_example() {
        let wasm_bytes = compile_and_validate("algorithms.go");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).unwrap();
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).unwrap();

        let is_prime = get_func::<i32, i32>(&instance, &mut store, "is_prime");
        assert_eq!(is_prime.call(&mut store, 2).unwrap(), 1);
        assert_eq!(is_prime.call(&mut store, 17).unwrap(), 1);
        assert_eq!(is_prime.call(&mut store, 4).unwrap(), 0);

        let gcd = get_func::<(i32, i32), i32>(&instance, &mut store, "gcd");
        assert_eq!(gcd.call(&mut store, (48, 18)).unwrap(), 6);

        let lcm = get_func::<(i32, i32), i32>(&instance, &mut store, "lcm");
        assert_eq!(lcm.call(&mut store, (12, 18)).unwrap(), 36);

        let is_palindrome_number =
            get_func::<i32, i32>(&instance, &mut store, "is_palindrome_number");
        assert_eq!(is_palindrome_number.call(&mut store, 121).unwrap(), 1);
        assert_eq!(is_palindrome_number.call(&mut store, 123).unwrap(), 0);

        let sum_of_digits = get_func::<i32, i32>(&instance, &mut store, "sum_of_digits");
        assert_eq!(sum_of_digits.call(&mut store, 123).unwrap(), 6);

        let count_digits = get_func::<i32, i32>(&instance, &mut store, "count_digits");
        assert_eq!(count_digits.call(&mut store, 12345).unwrap(), 5);

        let reverse_number = get_func::<i32, i32>(&instance, &mut store, "reverse_number");
        assert_eq!(reverse_number.call(&mut store, 123).unwrap(), 321);

        let binary_search = get_func::<i32, i32>(&instance, &mut store, "binary_search");
        assert_eq!(binary_search.call(&mut store, 7).unwrap(), 3);
        assert_eq!(binary_search.call(&mut store, 99).unwrap(), -1);

        let nth_triangular = get_func::<i32, i32>(&instance, &mut store, "nth_triangular");
        assert_eq!(nth_triangular.call(&mut store, 5).unwrap(), 15);

        let collatz_steps = get_func::<i32, i32>(&instance, &mut store, "collatz_steps");
        assert_eq!(collatz_steps.call(&mut store, 10).unwrap(), 6);

        let sum_of_squares = get_func::<i32, i32>(&instance, &mut store, "sum_of_squares");
        assert_eq!(sum_of_squares.call(&mut store, 3).unwrap(), 14);
    }

    #[test]
    fn test_combined_example() {
        let wasm_bytes = compile_and_validate("combined.go");

        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).unwrap();
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).unwrap();

        let analyze_array = get_func::<(), i32>(&instance, &mut store, "analyze_array");
        let result = analyze_array.call(&mut store, ()).unwrap();
        assert!(result > 0);

        let grade_calculator = get_func::<i32, i32>(&instance, &mut store, "grade_calculator");
        assert_eq!(grade_calculator.call(&mut store, 95).unwrap(), 4);
        assert_eq!(grade_calculator.call(&mut store, 85).unwrap(), 3);
        assert_eq!(grade_calculator.call(&mut store, 50).unwrap(), 0);

        let validate_and_process =
            get_func::<i32, i32>(&instance, &mut store, "validate_and_process");
        assert_eq!(validate_and_process.call(&mut store, -5).unwrap(), -1);
        assert_eq!(validate_and_process.call(&mut store, 5).unwrap(), 50);

        let filter_and_count = get_func::<(), i32>(&instance, &mut store, "filter_and_count");
        let count = filter_and_count.call(&mut store, ()).unwrap();
        assert!(count > 0);

        let string_category = get_func::<(), i32>(&instance, &mut store, "string_category");
        assert_eq!(string_category.call(&mut store, ()).unwrap(), 111);

        let complex_calculation =
            get_func::<(i32, i32, i32), i32>(&instance, &mut store, "complex_calculation");
        assert_eq!(complex_calculation.call(&mut store, (3, 4, 1)).unwrap(), 25);

        let array_transform = get_func::<i32, i32>(&instance, &mut store, "array_transform");
        assert_eq!(array_transform.call(&mut store, 1).unwrap(), 30);

        let point_distance_category =
            get_func::<(i32, i32, i32, i32), i32>(&instance, &mut store, "point_distance_category");
        assert_eq!(
            point_distance_category
                .call(&mut store, (0, 0, 3, 4))
                .unwrap(),
            2
        );
    }

    #[test]
    fn test_all_examples_compile() {
        // Verify all example files compile without errors
        let examples = vec![
            "calculator.go",
            "arrays.go",
            "strings.go",
            "structs.go",
            "switch.go",
            "algorithms.go",
            "combined.go",
        ];

        for example in examples {
            compile_and_validate(example);
        }
    }

    #[test]
    fn test_examples_generate_valid_wasm() {
        // Verify all WASM modules can be instantiated
        let examples = vec![
            "calculator.go",
            "arrays.go",
            "strings.go",
            "structs.go",
            "switch.go",
            "algorithms.go",
            "combined.go",
        ];

        let engine = Engine::default();

        for example in examples {
            let wasm_bytes = compile_and_validate(example);
            let module = Module::from_binary(&engine, &wasm_bytes)
                .unwrap_or_else(|e| panic!("Failed to create module from {}: {}", example, e));
            let mut store = Store::new(&engine, ());
            Instance::new(&mut store, &module, &[])
                .unwrap_or_else(|e| panic!("Failed to instantiate {}: {}", example, e));
        }
    }
}
