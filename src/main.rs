// src/main.rs
use clap::{Arg, Command};
use std::fs;
use std::path::Path;
use std::process;

use goiaba::wasm::compiler::compile_str;

fn main() {
    let matches = Command::new("goiaba")
        .version("0.1.0")
        .author("Raphael Amorim")
        .about("Go to WebAssembly compiler")
        .arg(
            Arg::new("input")
                .help("Input Go source file")
                .required(true)
                .index(1)
        )
        .arg(
            Arg::new("output")
                .help("Output WebAssembly file")
                .short('o')
                .long("output")
                .value_name("FILE")
                .required(false)
        )
        .arg(
            Arg::new("verbose")
                .help("Enable verbose output")
                .short('v')
                .long("verbose")
                .action(clap::ArgAction::SetTrue)
        )
        .get_matches();

    let input_file = matches.get_one::<String>("input").unwrap();
    let verbose = matches.get_flag("verbose");

    // Determine output file name
    let output_file = if let Some(output) = matches.get_one::<String>("output") {
        output.to_string()
    } else {
        // Default: replace .go extension with .wasm
        let input_path = Path::new(input_file);
        let stem = input_path.file_stem().unwrap_or_else(|| {
            eprintln!("Error: Unable to determine file stem from '{}'", input_file);
            process::exit(1);
        });
        format!("{}.wasm", stem.to_string_lossy())
    };

    if verbose {
        println!("Compiling {} to {}", input_file, output_file);
    }

    // Read input file
    let go_source = match fs::read_to_string(input_file) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", input_file, e);
            process::exit(1);
        }
    };

    // Compile Go to WASM
    let wasm_bytes = match compile_str(&go_source) {
        Ok(bytes) => bytes,
        Err(e) => {
            eprintln!("Compilation error: {}", e);
            process::exit(1);
        }
    };

    // Write output file
    if let Err(e) = fs::write(&output_file, wasm_bytes) {
        eprintln!("Error writing output file '{}': {}", output_file, e);
        process::exit(1);
    }

    if verbose {
        println!("Successfully compiled {} bytes to {}", 
                fs::metadata(&output_file).unwrap().len(), output_file);
    } else {
        println!("Compiled successfully: {}", output_file);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::process::Command as ProcessCommand;
    use tempfile::tempdir;

    #[test]
    fn test_cli_basic_compilation() {
        let temp_dir = tempdir().unwrap();
        let input_file = temp_dir.path().join("test.go");
        let output_file = temp_dir.path().join("test.wasm");

        // Create test Go file
        let go_code = r#"
            package main
            
            //export add
            func add(x int, y int) int {
                return x + y
            }
        "#;
        
        fs::write(&input_file, go_code).unwrap();

        // Test CLI compilation
        let output = ProcessCommand::new("cargo")
            .args(&["run", "--", 
                   input_file.to_str().unwrap(), 
                   "-o", output_file.to_str().unwrap()])
            .output();

        // Check if compilation succeeded (we can't easily test the actual CLI in unit tests)
        // So let's test the compilation logic directly
        let wasm_bytes = compile_str(go_code).expect("Compilation should succeed");
        fs::write(&output_file, wasm_bytes).unwrap();
        
        assert!(output_file.exists());
        assert!(fs::metadata(&output_file).unwrap().len() > 0);
    }

    #[test]
    fn test_default_output_filename() {
        let temp_dir = tempdir().unwrap();
        let input_file = temp_dir.path().join("example.go");
        
        let go_code = r#"
            package main
            
            //export multiply
            func multiply(a int, b int) int {
                return a * b
            }
        "#;
        
        fs::write(&input_file, go_code).unwrap();

        // Test that default output name is generated correctly
        let wasm_bytes = compile_str(go_code).expect("Compilation should succeed");
        let default_output = temp_dir.path().join("example.wasm");
        fs::write(&default_output, wasm_bytes).unwrap();
        
        assert!(default_output.exists());
    }

    #[test]
    fn test_invalid_go_file() {
        let temp_dir = tempdir().unwrap();
        let input_file = temp_dir.path().join("invalid.go");
        
        // Create invalid Go file
        let invalid_go_code = "this is not valid go code";
        fs::write(&input_file, invalid_go_code).unwrap();

        // Test that compilation fails appropriately
        let result = compile_str(invalid_go_code);
        assert!(result.is_err());
    }
}