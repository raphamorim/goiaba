// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

use clap::{Arg, Command};
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use goiaba::wasm::compiler::compile_str;
use goiaba::parser::parse_str;

use goiaba::bindings::JSBindingGenerator;

fn main() {
    let matches = Command::new("goiaba")
        .version("0.1.0")
        .author("Raphael Amorim")
        .about("Go to WebAssembly compiler with web project generation")
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
            Arg::new("web")
                .help("Generate web project with HTML and JS bindings")
                .short('w')
                .long("web")
                .value_name("DIR")
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

    if verbose {
        println!("Compiling Go file: {}", input_file);
    }

    // Read and parse Go source
    let go_source = match fs::read_to_string(input_file) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", input_file, e);
            process::exit(1);
        }
    };

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
    let input_path = Path::new(input_file);
    let default_output = input_path.with_extension("wasm");
    let output_file = matches.get_one::<String>("output")
        .map(|s| s.clone())
        .unwrap_or_else(|| default_output.to_string_lossy().to_string());

    // Write WASM file
    if let Err(e) = fs::write(&output_file, &wasm_bytes) {
        eprintln!("Error writing WASM file '{}': {}", output_file, e);
        process::exit(1);
    }

    if verbose {
        println!("Generated WASM: {} ({} bytes)", output_file, wasm_bytes.len());
    }

    // Check if web project generation is requested
    if let Some(web_dir) = matches.get_one::<String>("web") {
        generate_web_project(
            web_dir,
            &output_file,
            &go_source,
            &go_program,
            &ast_objects,
            input_path,
            verbose
        );
    } else {
        println!("Successfully compiled: {}", output_file);
    }
}

fn generate_web_project(
    web_dir: &str,
    wasm_filename: &str,
    go_source: &str,
    go_program: &goiaba::parser::ast::File,
    ast_objects: &goiaba::parser::objects::AstObjects,
    input_path: &Path,
    verbose: bool
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

    let project_name = input_path.file_stem()
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
    format!(r#"{{
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
}}"#, project_name.replace(" ", "-").to_lowercase())
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
            false
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
}