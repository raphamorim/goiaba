// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

use goiaba::parser::parse_str;
use goiaba::wasm::compiler::{GoToWasmTranslator, WasmType, compile_str};
use wasmtime::{Engine, Instance, Module, Store};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_struct_with_bool_fields() {
        let go_source = r#"
            package main
            
            type Flags struct {
                isActive bool
                isEnabled bool
            }
            
            //export check_flags
            func check_flags(active int, enabled int) int {
                f := Flags{isActive: active != 0, isEnabled: enabled != 0}
                
                if f.isActive && f.isEnabled {
                    return 1
                }
                return 0
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load module");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("Failed to instantiate");

        let check_flags = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "check_flags")
            .expect("Failed to get function");

        assert_eq!(check_flags.call(&mut store, (1, 1)).unwrap(), 1);
        assert_eq!(check_flags.call(&mut store, (1, 0)).unwrap(), 0);
        assert_eq!(check_flags.call(&mut store, (0, 1)).unwrap(), 0);
    }

    #[test]
    fn test_struct_with_string_fields() {
        let go_source = r#"
            package main
            
            type Person struct {
                name string
                age int
            }
            
            //export get_age
            func get_age() int {
                p := Person{name: "Alice", age: 30}
                return p.age
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load module");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("Failed to instantiate");

        let get_age = instance
            .get_typed_func::<(), i32>(&mut store, "get_age")
            .expect("Failed to get function");

        assert_eq!(get_age.call(&mut store, ()).unwrap(), 30);
    }

    #[test]
    fn test_struct_with_multiple_integer_types() {
        let go_source = r#"
            package main
            
            type Numbers struct {
                small int8
                medium int16
                large int64
                normal int
            }
            
            //export sum_numbers
            func sum_numbers(a int, b int, c int, d int) int {
                n := Numbers{small: int8(a), medium: int16(b), large: int64(c), normal: d}
                return int(n.small) + int(n.medium) + int(n.large) + n.normal
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load module");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("Failed to instantiate");

        let sum_numbers = instance
            .get_typed_func::<(i32, i32, i32, i32), i32>(&mut store, "sum_numbers")
            .expect("Failed to get function");

        assert_eq!(sum_numbers.call(&mut store, (1, 2, 3, 4)).unwrap(), 10);
    }

    #[test]
    fn test_struct_with_unsigned_types() {
        let go_source = r#"
            package main
            
            type UnsignedData struct {
                byte8 uint8
                short16 uint16
                int32 uint32
            }
            
            //export process_unsigned
            func process_unsigned(a int, b int, c int) int {
                data := UnsignedData{byte8: uint8(a), short16: uint16(b), int32: uint32(c)}
                return int(data.byte8) + int(data.short16) + int(data.int32)
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load module");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("Failed to instantiate");

        let process_unsigned = instance
            .get_typed_func::<(i32, i32, i32), i32>(&mut store, "process_unsigned")
            .expect("Failed to get function");

        assert_eq!(process_unsigned.call(&mut store, (10, 20, 30)).unwrap(), 60);
    }

    #[test]
    fn test_struct_with_mixed_types() {
        let go_source = r#"
            package main
            
            type MixedData struct {
                count int
                isValid bool
                id uint32
                value int64
            }
            
            //export compute_mixed
            func compute_mixed(count int, valid int, id int, value int) int {
                data := MixedData{
                    count: count,
                    isValid: valid != 0,
                    id: uint32(id),
                    value: int64(value),
                }
                
                result := data.count + int(data.id) + int(data.value)
                if data.isValid {
                    result = result * 2
                }
                return result
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load module");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("Failed to instantiate");

        let compute_mixed = instance
            .get_typed_func::<(i32, i32, i32, i32), i32>(&mut store, "compute_mixed")
            .expect("Failed to get function");

        // When valid=1, result should be (5 + 10 + 15) * 2 = 60
        assert_eq!(compute_mixed.call(&mut store, (5, 1, 10, 15)).unwrap(), 60);
        // When valid=0, result should be (5 + 10 + 15) = 30
        assert_eq!(compute_mixed.call(&mut store, (5, 0, 10, 15)).unwrap(), 30);
    }

    #[test]
    fn test_nested_structs() {
        let go_source = r#"
            package main
            
            type Inner struct {
                value int
            }
            
            type Outer struct {
                data Inner
                multiplier int
            }
            
            //export process_nested
            func process_nested(val int, mult int) int {
                inner := Inner{value: val}
                outer := Outer{data: inner, multiplier: mult}
                return outer.data.value * outer.multiplier
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load module");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("Failed to instantiate");

        let process_nested = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "process_nested")
            .expect("Failed to get function");

        assert_eq!(process_nested.call(&mut store, (7, 3)).unwrap(), 21);
        assert_eq!(process_nested.call(&mut store, (10, 5)).unwrap(), 50);
    }

    #[test]
    fn test_struct_field_assignment_with_different_types() {
        let go_source = r#"
            package main
            
            type Counter struct {
                current int
                maximum int
                active bool
            }
            
            //export update_counter
            func update_counter(initial int, max int) int {
                c := Counter{current: initial, maximum: max, active: true}
                
                c.current = c.current + 5
                
                if c.current > c.maximum {
                    c.active = false
                    return -1
                }
                
                return c.current
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load module");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("Failed to instantiate");

        let update_counter = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "update_counter")
            .expect("Failed to get function");

        assert_eq!(update_counter.call(&mut store, (3, 20)).unwrap(), 8);
        assert_eq!(update_counter.call(&mut store, (15, 10)).unwrap(), -1);
    }

    #[test]
    fn test_struct_with_byte_type() {
        let go_source = r#"
            package main
            
            type ByteData struct {
                b byte
                count int
            }
            
            //export process_byte
            func process_byte(byteVal int, count int) int {
                data := ByteData{b: byte(byteVal), count: count}
                return int(data.b) * data.count
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load module");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("Failed to instantiate");

        let process_byte = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "process_byte")
            .expect("Failed to get function");

        assert_eq!(process_byte.call(&mut store, (5, 3)).unwrap(), 15);
        assert_eq!(process_byte.call(&mut store, (10, 2)).unwrap(), 20);
    }

    #[test]
    fn test_multiple_structs_different_types() {
        let go_source = r#"
            package main
            
            type TypeA struct {
                x int
                y int
            }
            
            type TypeB struct {
                a bool
                b uint32
            }
            
            //export combine_structs
            func combine_structs(x int, y int, valid int, id int) int {
                ta := TypeA{x: x, y: y}
                tb := TypeB{a: valid != 0, b: uint32(id)}
                
                sum := ta.x + ta.y + int(tb.b)
                if tb.a {
                    sum = sum * 2
                }
                return sum
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load module");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("Failed to instantiate");

        let combine_structs = instance
            .get_typed_func::<(i32, i32, i32, i32), i32>(&mut store, "combine_structs")
            .expect("Failed to get function");

        // (10 + 20 + 30) * 2 = 120 when valid=1
        assert_eq!(
            combine_structs.call(&mut store, (10, 20, 1, 30)).unwrap(),
            120
        );
        // (10 + 20 + 30) = 60 when valid=0
        assert_eq!(
            combine_structs.call(&mut store, (10, 20, 0, 30)).unwrap(),
            60
        );
    }

    #[test]
    fn test_struct_type_conversions() {
        let go_source = r#"
            package main
            
            type ConversionTest struct {
                i8 int8
                i16 int16
                i32 int
                i64 int64
            }
            
            //export test_conversions
            func test_conversions(val int) int {
                ct := ConversionTest{
                    i8: int8(val),
                    i16: int16(val * 2),
                    i32: val * 3,
                    i64: int64(val * 4),
                }
                return int(ct.i8) + int(ct.i16) + ct.i32 + int(ct.i64)
            }
        "#;

        let wasm_bytes = compile_str(go_source).expect("Failed to compile");
        let engine = Engine::default();
        let module = Module::from_binary(&engine, &wasm_bytes).expect("Failed to load module");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("Failed to instantiate");

        let test_conversions = instance
            .get_typed_func::<i32, i32>(&mut store, "test_conversions")
            .expect("Failed to get function");

        // 5 + 10 + 15 + 20 = 50
        assert_eq!(test_conversions.call(&mut store, 5).unwrap(), 50);
        // 2 + 4 + 6 + 8 = 20
        assert_eq!(test_conversions.call(&mut store, 2).unwrap(), 20);
    }

    // Tests to verify WASM type information
    mod wasm_type_verification {
        use super::*;

        #[test]
        fn test_verify_struct_field_types_in_ast() {
            let go_source = r#"
                package main
                
                type TypedStruct struct {
                    b bool
                    i8 int8
                    i16 int16
                    i32 int
                    i64 int64
                    u8 uint8
                    u16 uint16
                    u32 uint32
                    u64 uint64
                    f32 float32
                    f64 float64
                    s string
                }
                
                func dummy() int {
                    return 0
                }
            "#;

            let (ast_objects, go_program) = parse_str(go_source).expect("Failed to parse");
            let wasm_program =
                GoToWasmTranslator::translate_program(&go_program, &ast_objects, go_source);

            // Find the TypedStruct definition
            let typed_struct = wasm_program
                .structs
                .iter()
                .find(|s| s.name == "TypedStruct")
                .expect("TypedStruct not found");

            // Verify field types
            let field_types: std::collections::HashMap<&str, &WasmType> = typed_struct
                .fields
                .iter()
                .map(|(name, typ)| (name.as_str(), typ))
                .collect();

            assert_eq!(field_types.get("b"), Some(&&WasmType::Bool));
            assert_eq!(field_types.get("i8"), Some(&&WasmType::Int8));
            assert_eq!(field_types.get("i16"), Some(&&WasmType::Int16));
            assert_eq!(field_types.get("i32"), Some(&&WasmType::Int));
            assert_eq!(field_types.get("i64"), Some(&&WasmType::Int64));
            assert_eq!(field_types.get("u8"), Some(&&WasmType::Uint8));
            assert_eq!(field_types.get("u16"), Some(&&WasmType::Uint16));
            assert_eq!(field_types.get("u32"), Some(&&WasmType::Uint32));
            assert_eq!(field_types.get("u64"), Some(&&WasmType::Uint64));
            assert_eq!(field_types.get("f32"), Some(&&WasmType::Float));
            assert_eq!(field_types.get("f64"), Some(&&WasmType::Float64));
            assert_eq!(field_types.get("s"), Some(&&WasmType::String));
        }

        #[test]
        fn test_verify_struct_field_offsets() {
            let go_source = r#"
                package main
                
                type OffsetTest struct {
                    a int8      // offset 0, size 1
                    b int16     // offset 1, size 2
                    c int32     // offset 3, size 4
                    d int64     // offset 7, size 8
                }
                
                func dummy() int {
                    return 0
                }
            "#;

            let (ast_objects, go_program) = parse_str(go_source).expect("Failed to parse");
            let wasm_program =
                GoToWasmTranslator::translate_program(&go_program, &ast_objects, go_source);

            let offset_test = wasm_program
                .structs
                .iter()
                .find(|s| s.name == "OffsetTest")
                .expect("OffsetTest not found");

            // Verify field offsets
            assert_eq!(offset_test.field_offsets.get("a"), Some(&0));
            assert_eq!(offset_test.field_offsets.get("b"), Some(&1));
            assert_eq!(offset_test.field_offsets.get("c"), Some(&3));
            assert_eq!(offset_test.field_offsets.get("d"), Some(&7));

            // Verify total struct size: 1 + 2 + 4 + 8 = 15 bytes
            assert_eq!(offset_test.size, 15);
        }

        #[test]
        fn test_verify_mixed_size_struct() {
            let go_source = r#"
                package main
                
                type MixedSize struct {
                    small uint8     // 1 byte
                    medium uint16   // 2 bytes
                    large uint32    // 4 bytes
                    huge uint64     // 8 bytes
                }
                
                func dummy() int {
                    return 0
                }
            "#;

            let (ast_objects, go_program) = parse_str(go_source).expect("Failed to parse");
            let wasm_program =
                GoToWasmTranslator::translate_program(&go_program, &ast_objects, go_source);

            let mixed_size = wasm_program
                .structs
                .iter()
                .find(|s| s.name == "MixedSize")
                .expect("MixedSize not found");

            // Verify offsets: 0, 1, 3, 7
            assert_eq!(mixed_size.field_offsets.get("small"), Some(&0));
            assert_eq!(mixed_size.field_offsets.get("medium"), Some(&1));
            assert_eq!(mixed_size.field_offsets.get("large"), Some(&3));
            assert_eq!(mixed_size.field_offsets.get("huge"), Some(&7));

            // Verify total size: 1 + 2 + 4 + 8 = 15 bytes
            assert_eq!(mixed_size.size, 15);
        }

        #[test]
        fn test_verify_bool_type() {
            let go_source = r#"
                package main
                
                type BoolStruct struct {
                    flag1 bool
                    flag2 bool
                    value int
                }
                
                func dummy() int {
                    return 0
                }
            "#;

            let (ast_objects, go_program) = parse_str(go_source).expect("Failed to parse");
            let wasm_program =
                GoToWasmTranslator::translate_program(&go_program, &ast_objects, go_source);

            let bool_struct = wasm_program
                .structs
                .iter()
                .find(|s| s.name == "BoolStruct")
                .expect("BoolStruct not found");

            // Verify bool fields have Bool type
            let field_types: std::collections::HashMap<&str, &WasmType> = bool_struct
                .fields
                .iter()
                .map(|(name, typ)| (name.as_str(), typ))
                .collect();

            assert_eq!(field_types.get("flag1"), Some(&&WasmType::Bool));
            assert_eq!(field_types.get("flag2"), Some(&&WasmType::Bool));
            assert_eq!(field_types.get("value"), Some(&&WasmType::Int));

            // Verify offsets: bool is 1 byte
            assert_eq!(bool_struct.field_offsets.get("flag1"), Some(&0));
            assert_eq!(bool_struct.field_offsets.get("flag2"), Some(&1));
            assert_eq!(bool_struct.field_offsets.get("value"), Some(&2));

            // Total size: 1 + 1 + 4 = 6 bytes
            assert_eq!(bool_struct.size, 6);
        }

        #[test]
        fn test_verify_byte_type() {
            let go_source = r#"
                package main
                
                type ByteStruct struct {
                    b byte
                    count int
                }
                
                func dummy() int {
                    return 0
                }
            "#;

            let (ast_objects, go_program) = parse_str(go_source).expect("Failed to parse");
            let wasm_program =
                GoToWasmTranslator::translate_program(&go_program, &ast_objects, go_source);

            let byte_struct = wasm_program
                .structs
                .iter()
                .find(|s| s.name == "ByteStruct")
                .expect("ByteStruct not found");

            // Verify byte is treated as Uint8
            let field_types: std::collections::HashMap<&str, &WasmType> = byte_struct
                .fields
                .iter()
                .map(|(name, typ)| (name.as_str(), typ))
                .collect();

            assert_eq!(field_types.get("b"), Some(&&WasmType::Uint8));
            assert_eq!(field_types.get("count"), Some(&&WasmType::Int));
        }
    }
}
