// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

use wasm_encoder::ValType;

/// Represents a stdlib function import
pub struct StdlibImport {
    pub module: String,
    pub name: String,
    pub params: Vec<ValType>,
    pub results: Vec<ValType>,
}

/// Check if a function call is a supported stdlib function
/// Returns the import details if supported, None otherwise
pub fn get_stdlib_import(pkg: &str, func: &str) -> Option<StdlibImport> {
    match (pkg, func) {
        ("math", "Sqrt") => Some(StdlibImport {
            module: "env".to_string(),
            name: "sqrt".to_string(),
            params: vec![ValType::F64],
            results: vec![ValType::F64],
        }),
        ("strings", "Len") => Some(StdlibImport {
            module: "env".to_string(),
            name: "strings_len".to_string(),
            params: vec![ValType::I32, ValType::I32], // string ptr, len
            results: vec![ValType::I32],              // length
        }),
        ("strings", "Join") => Some(StdlibImport {
            module: "env".to_string(),
            name: "strings_join".to_string(),
            params: vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32], // slice ptr, len, sep ptr, len
            results: vec![ValType::I32, ValType::I32], // result string ptr, len
        }),
        ("fmt", "Println") => Some(StdlibImport {
            module: "env".to_string(),
            name: "print".to_string(),
            params: vec![ValType::I32, ValType::I32], // string ptr, len
            results: vec![],
        }),
        // Add more stdlib functions here
        _ => None,
    }
}

/// Get the list of all supported stdlib packages
pub fn get_supported_packages() -> Vec<&'static str> {
    vec!["math", "strings", "fmt"]
}

/// Check if a package is a supported stdlib package
pub fn is_supported_package(pkg: &str) -> bool {
    get_supported_packages().contains(&pkg)
}
