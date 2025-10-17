// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

use clap::{Arg, Command};
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use goiaba::parser::parse_str;
use goiaba::wasm::compiler::compile_str;

use goiaba::bindings::JSBindingGenerator;

fn scan_go_files(directory: &str) -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let mut go_files = Vec::new();

    for entry in fs::read_dir(directory)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_file()
            && let Some(extension) = path.extension()
            && extension == "go"
            && let Some(path_str) = path.to_str()
        {
            go_files.push(path_str.to_string());
        }
    }

    if go_files.is_empty() {
        return Err(format!("No .go files found in directory '{}'", directory).into());
    }

    // Sort for consistent ordering
    go_files.sort();
    Ok(go_files)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let app = Command::new("goiaba")
        .version("0.1.0")
        .author("Raphael Amorim")
        .about("Go to WebAssembly compiler with web project generation")
        .subcommand_required(false)
        .arg_required_else_help(false)
        .arg(
            Arg::new("input")
                .help("Input Go source files")
                .required(false)
                .num_args(1..)
                .index(1),
        )
        .arg(
            Arg::new("output")
                .help("Output WebAssembly file")
                .short('o')
                .long("output")
                .value_name("FILE")
                .required(false),
        )
        .arg(
            Arg::new("web")
                .help("Generate web project with HTML and JS bindings")
                .short('w')
                .long("web")
                .value_name("DIR")
                .required(false),
        )
        .arg(
            Arg::new("verbose")
                .help("Enable verbose output")
                .short('v')
                .long("verbose")
                .action(clap::ArgAction::SetTrue),
        )
        .subcommand(
            Command::new("compile")
                .about("Compile Go source files")
                .arg(
                    Arg::new("input")
                        .help("Input Go source files")
                        .required(true)
                        .num_args(1..)
                        .index(1),
                )
                .arg(
                    Arg::new("output")
                        .help("Output WebAssembly file")
                        .short('o')
                        .long("output")
                        .value_name("FILE")
                        .required(false),
                )
                .arg(
                    Arg::new("web")
                        .help("Generate web project with HTML and JS bindings")
                        .short('w')
                        .long("web")
                        .value_name("DIR")
                        .required(false),
                )
                .arg(
                    Arg::new("verbose")
                        .help("Enable verbose output")
                        .short('v')
                        .long("verbose")
                        .action(clap::ArgAction::SetTrue),
                ),
        )
        .subcommand(
            Command::new("build")
                .about("Build a Go package from a directory")
                .arg(
                    Arg::new("directory")
                        .help("Directory containing Go source files")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::new("output")
                        .help("Output WebAssembly file")
                        .short('o')
                        .long("output")
                        .value_name("FILE")
                        .required(false),
                )
                .arg(
                    Arg::new("web")
                        .help("Generate web project with HTML and JS bindings")
                        .short('w')
                        .long("web")
                        .value_name("DIR")
                        .required(false),
                )
                .arg(
                    Arg::new("verbose")
                        .help("Enable verbose output")
                        .short('v')
                        .long("verbose")
                        .action(clap::ArgAction::SetTrue),
                ),
        );

    let matches = app.get_matches();

    let (input_files, verbose, output_file, web_dir) = if let Some(compile_matches) =
        matches.subcommand_matches("compile")
    {
        let input_files: Vec<String> = compile_matches
            .get_many::<String>("input")
            .unwrap()
            .cloned()
            .collect();
        let verbose = compile_matches.get_flag("verbose");
        let output_file = compile_matches.get_one::<String>("output").cloned();
        let web_dir = compile_matches.get_one::<String>("web").cloned();
        (input_files, verbose, output_file, web_dir)
    } else if let Some(build_matches) = matches.subcommand_matches("build") {
        let directory = build_matches.get_one::<String>("directory").unwrap();
        let verbose = build_matches.get_flag("verbose");
        let output_file = build_matches.get_one::<String>("output").cloned();
        let web_dir = build_matches.get_one::<String>("web").cloned();

        // Scan directory for .go files
        let input_files = scan_go_files(directory)?;
        (input_files, verbose, output_file, web_dir)
    } else if matches.contains_id("input") {
        // Default behavior: treat as compile command (backward compatibility)
        let input_files: Vec<String> = matches
            .get_many::<String>("input")
            .unwrap()
            .cloned()
            .collect();
        let verbose = matches.get_flag("verbose");
        let output_file = matches.get_one::<String>("output").cloned();
        let web_dir = matches.get_one::<String>("web").cloned();
        (input_files, verbose, output_file, web_dir)
    } else {
        eprintln!(
            "Error: No input files specified. Use 'goiaba compile <files...>' or 'goiaba build <directory>'"
        );
        process::exit(1);
    };

    if verbose {
        println!("Compiling Go files: {}", input_files.join(", "));
    }

    // Read and concatenate all Go source files
    let mut go_source = String::new();
    for input_file in &input_files {
        let content = match fs::read_to_string(input_file) {
            Ok(content) => content,
            Err(e) => {
                eprintln!("Error reading file '{}': {}", input_file, e);
                process::exit(1);
            }
        };
        go_source.push_str(&content);
        go_source.push('\n'); // Add newline between files
    }

    // Parse Go source to extract function information
    let (ast_objects, go_program) = match parse_str(&go_source) {
        Ok((objs, prog)) => (objs, prog),
        Err(e) => {
            eprintln!("Parse error: {}", e);
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

    // Determine output file name
    let first_input_file = &input_files[0];
    let input_path = Path::new(first_input_file);
    let default_output = input_path.with_extension("wasm");
    let output_file = output_file.unwrap_or_else(|| default_output.to_string_lossy().to_string());

    // Write WASM file
    if let Err(e) = fs::write(&output_file, &wasm_bytes) {
        eprintln!("Error writing WASM file '{}': {}", output_file, e);
        process::exit(1);
    }

    if verbose {
        println!(
            "Generated WASM: {} ({} bytes)",
            output_file,
            wasm_bytes.len()
        );
    }

    // Check if web project generation is requested
    if let Some(ref web_dir_str) = web_dir {
        generate_web_project(
            web_dir_str,
            &output_file,
            &go_source,
            &go_program,
            &ast_objects,
            input_path,
            verbose,
        );
    } else {
        println!("Successfully compiled: {}", output_file);
    }

    Ok(())
}

fn generate_web_project(
    web_dir: &str,
    wasm_filename: &str,
    go_source: &str,
    go_program: &goiaba::parser::ast::File,
    ast_objects: &goiaba::parser::objects::AstObjects,
    input_path: &Path,
    verbose: bool,
) {
    let web_path = PathBuf::from(web_dir);

    // Create web directory
    if let Err(e) = fs::create_dir_all(&web_path) {
        eprintln!("Error creating web directory '{}': {}", web_dir, e);
        process::exit(1);
    }

    // Initialize JS binding generator
    let mut js_gen = JSBindingGenerator::new();
    js_gen.analyze_go_source(go_source, go_program, ast_objects);

    let project_name = input_path
        .file_stem()
        .unwrap_or_else(|| std::ffi::OsStr::new("go-wasm-project"))
        .to_string_lossy()
        .to_string();

    let wasm_file_name = Path::new(wasm_filename)
        .file_name()
        .unwrap()
        .to_string_lossy()
        .to_string();

    // Copy WASM file to web directory
    let web_wasm_path = web_path.join(&wasm_file_name);
    if let Err(e) = fs::copy(wasm_filename, &web_wasm_path) {
        eprintln!("Error copying WASM file: {}", e);
        process::exit(1);
    }

    // Generate HTML file
    let html_content = js_gen.generate_html(&wasm_file_name, &project_name);
    let html_path = web_path.join("index.html");
    if let Err(e) = fs::write(&html_path, html_content) {
        eprintln!("Error writing HTML file: {}", e);
        process::exit(1);
    }

    // Generate JavaScript file
    let js_content = js_gen.generate_javascript(&wasm_file_name);
    let js_path = web_path.join("main.js");
    if let Err(e) = fs::write(&js_path, js_content) {
        eprintln!("Error writing JavaScript file: {}", e);
        process::exit(1);
    }

    // Generate TypeScript definitions
    let ts_content = js_gen.generate_typescript_definitions();
    let ts_path = web_path.join("types.d.ts");
    if let Err(e) = fs::write(&ts_path, ts_content) {
        eprintln!("Error writing TypeScript definitions: {}", e);
        process::exit(1);
    }

    // Generate README
    let readme_content = js_gen.generate_readme(&project_name, &wasm_file_name);
    let readme_path = web_path.join("README.md");
    if let Err(e) = fs::write(&readme_path, readme_content) {
        eprintln!("Error writing README file: {}", e);
        process::exit(1);
    }

    // Generate package.json for npm projects
    let package_json = generate_package_json(&project_name);
    let package_path = web_path.join("package.json");
    if let Err(e) = fs::write(&package_path, package_json) {
        eprintln!("Error writing package.json: {}", e);
        process::exit(1);
    }

    if verbose {
        println!("Generated web project files:");
        println!("  📁 {}/", web_dir);
        println!("    📄 index.html - Interactive demo page");
        println!("    📄 main.js - JavaScript bindings");
        println!("    📄 types.d.ts - TypeScript definitions");
        println!("    📄 {} - WebAssembly module", wasm_file_name);
        println!("    📄 README.md - Documentation");
        println!("    📄 package.json - NPM configuration");
    }

    println!("✅ Web project generated in: {}", web_dir);
    println!("🚀 To run: cd {} && python -m http.server 8000", web_dir);
    println!("🌐 Then open: http://localhost:8000");
}

fn generate_package_json(project_name: &str) -> String {
    format!(
        r#"{{
  "name": "{}",
  "version": "1.0.0",
  "description": "Go functions compiled to WebAssembly",
  "main": "main.js",
  "scripts": {{
    "start": "python -m http.server 8000",
    "serve": "npx serve .",
    "dev": "npx live-server ."
  }},
  "keywords": ["webassembly", "wasm", "go", "golang"],
  "author": "Generated by Goiaba",
  "license": "MIT",
  "devDependencies": {{
    "live-server": "^1.2.2",
    "serve": "^14.2.1"
  }}
}}"#,
        project_name.replace(" ", "-").to_lowercase()
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_web_project_generation() {
        let temp_dir = tempdir().unwrap();
        let input_file = temp_dir.path().join("test.go");
        let web_dir = temp_dir.path().join("web");

        let go_code = r#"
            package main
            
            //export add
            func add(x int, y int) int {
                return x + y
            }
            
            //export multiply
            func multiply(a int, b int) int {
                return a * b
            }
        "#;

        fs::write(&input_file, go_code).unwrap();

        // Parse and compile
        let (ast_objects, go_program) = parse_str(go_code).unwrap();
        let wasm_bytes = compile_str(go_code).unwrap();
        let wasm_file = temp_dir.path().join("test.wasm");
        fs::write(&wasm_file, wasm_bytes).unwrap();

        // Generate web project
        generate_web_project(
            web_dir.to_str().unwrap(),
            wasm_file.to_str().unwrap(),
            go_code,
            &go_program,
            &ast_objects,
            &input_file,
            false,
        );

        // Verify generated files
        assert!(web_dir.join("index.html").exists());
        assert!(web_dir.join("main.js").exists());
        assert!(web_dir.join("types.d.ts").exists());
        assert!(web_dir.join("test.wasm").exists());
        assert!(web_dir.join("README.md").exists());
        assert!(web_dir.join("package.json").exists());

        // Verify HTML contains function demos
        let html_content = fs::read_to_string(web_dir.join("index.html")).unwrap();
        assert!(html_content.contains("add"));
        assert!(html_content.contains("multiply"));
        assert!(html_content.contains("Interactive demo"));

        // Verify JS contains function bindings
        let js_content = fs::read_to_string(web_dir.join("main.js")).unwrap();
        assert!(js_content.contains("call_add"));
        assert!(js_content.contains("call_multiply"));
        assert!(js_content.contains("loadWasm"));
    }

    #[test]
    fn test_multi_file_compilation() {
        let temp_dir = tempdir().unwrap();

        // Create first file with package declaration and one function
        let file1_path = temp_dir.path().join("file1.go");
        let file1_content = "package main\n\nfunc add(x int, y int) int {\n    return x + y\n}\n";
        fs::write(&file1_path, file1_content).unwrap();

        // Create second file with another function (no package declaration)
        let file2_path = temp_dir.path().join("file2.go");
        let file2_content = "func multiply(x int, y int) int {\n    return x * y\n}\n";
        fs::write(&file2_path, file2_content).unwrap();

        // Create third file with main function that uses both (no package declaration)
        let file3_path = temp_dir.path().join("file3.go");
        let file3_content = "//export calculate\nfunc calculate(a int, b int) int {\n    sum := add(a, b)\n    product := multiply(sum, 2)\n    return product\n}\n";
        fs::write(&file3_path, file3_content).unwrap();

        // Test concatenation by reading files manually
        let mut concatenated = String::new();
        for file_path in [&file1_path, &file2_path, &file3_path] {
            concatenated.push_str(&fs::read_to_string(file_path).unwrap());
            concatenated.push('\n');
        }

        // Compile the concatenated source
        let result = compile_str(&concatenated);
        assert!(
            result.is_ok(),
            "Failed to compile multi-file source: {:?}",
            result.err()
        );

        let wasm_bytes = result.unwrap();
        assert!(!wasm_bytes.is_empty(), "Generated WASM should not be empty");

        // Verify that functions from different files are present
        // We can't easily inspect the WASM without parsing, but successful compilation
        // indicates that all functions were parsed and compiled
    }

    #[test]
    fn test_single_file_still_works() {
        let temp_dir = tempdir().unwrap();
        let single_file_path = temp_dir.path().join("single.go");
        let single_file_content =
            "package main\n\n//export test\nfunc test(x int) int {\n    return x * 2\n}\n";
        fs::write(&single_file_path, single_file_content).unwrap();

        let result = compile_str(single_file_content);
        assert!(
            result.is_ok(),
            "Failed to compile single file: {:?}",
            result.err()
        );

        let wasm_bytes = result.unwrap();
        assert!(!wasm_bytes.is_empty(), "Generated WASM should not be empty");
    }

    #[test]
    fn test_empty_file_handling() {
        let temp_dir = tempdir().unwrap();

        // Create a file with only package declaration
        let file1_path = temp_dir.path().join("pkg.go");
        fs::write(&file1_path, "package main\n").unwrap();

        // Create a file with actual content (no package declaration)
        let file2_path = temp_dir.path().join("func.go");
        let file2_content = "//export test\nfunc test() int {\n    return 42\n}\n";
        fs::write(&file2_path, file2_content).unwrap();

        // Concatenate manually
        let mut concatenated = String::new();
        concatenated.push_str("package main\n");
        concatenated.push('\n');
        concatenated.push_str(file2_content);
        concatenated.push('\n');

        let result = compile_str(&concatenated);
        assert!(
            result.is_ok(),
            "Failed to compile with empty-ish files: {:?}",
            result.err()
        );

        let wasm_bytes = result.unwrap();
        assert!(!wasm_bytes.is_empty(), "Generated WASM should not be empty");
    }
}
