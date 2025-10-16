use crate::parser::ast::{Decl, File};
use crate::parser::objects::AstObjects;

#[derive(Debug, Clone)]
pub struct ExportedFunction {
    pub name: String,
    pub export_name: String,
    pub param_count: usize,
    pub param_names: Vec<String>,
    pub has_return: bool,
}

pub struct JSBindingGenerator {
    exported_functions: Vec<ExportedFunction>,
}

impl JSBindingGenerator {
    pub fn new() -> Self {
        Self {
            exported_functions: Vec::new(),
        }
    }

    pub fn analyze_go_source(&mut self, source: &str, ast: &File, objs: &AstObjects) {
        self.exported_functions.clear();

        for decl in &ast.decls {
            if let Decl::Func(func_key) = decl {
                let func_decl = &objs.fdecls[*func_key];

                if let Some(export_name) =
                    extract_export_name(source, &objs.idents[func_decl.name].name)
                {
                    let func_type = &objs.ftypes[func_decl.typ];

                    let mut param_names = Vec::new();
                    let mut param_count = 0;

                    for field_key in &func_type.params.list {
                        let field = &objs.fields[*field_key];
                        for name_key in &field.names {
                            param_names.push(objs.idents[*name_key].name.clone());
                            param_count += 1;
                        }
                    }

                    let has_return = func_type.results.is_some()
                        && !func_type.results.as_ref().unwrap().list.is_empty();

                    self.exported_functions.push(ExportedFunction {
                        name: objs.idents[func_decl.name].name.clone(),
                        export_name,
                        param_count,
                        param_names,
                        has_return,
                    });
                }
            }
        }
    }

    pub fn generate_html(&self, _wasm_filename: &str, project_name: &str) -> String {
        let function_demos = self.generate_function_demos();
        format!(
            r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{} - Go WASM Demo</title>
    <style>
        body {{
            font-family: Arial, sans-serif;
            max-width: 800px;
            margin: 0 auto;
            padding: 20px;
            background-color: #f5f5f5;
        }}
        
        .container {{
            background: white;
            border-radius: 10px;
            padding: 30px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }}
        
        h1 {{
            color: #333;
            border-bottom: 2px solid #007acc;
            padding-bottom: 10px;
        }}
        
        .function-demo {{
            margin: 20px 0;
            padding: 15px;
            border: 1px solid #ddd;
            border-radius: 5px;
            background: #f9f9f9;
        }}
        
        .function-title {{
            font-weight: bold;
            color: #007acc;
            margin-bottom: 10px;
        }}
        
        input[type="number"] {{
            width: 80px;
            padding: 5px;
            margin: 2px;
            border: 1px solid #ccc;
            border-radius: 3px;
        }}
        
        button {{
            background: #007acc;
            color: white;
            border: none;
            padding: 8px 15px;
            border-radius: 3px;
            cursor: pointer;
            margin-left: 10px;
        }}
        
        button:hover {{
            background: #005999;
        }}
        
        .result {{
            margin-top: 10px;
            padding: 10px;
            background: #e8f4fd;
            border-left: 4px solid #007acc;
            font-family: monospace;
        }}
        
        .error {{
            color: #d32f2f;
            background: #ffebee;
            border-left-color: #d32f2f;
        }}
        
        .loading {{
            color: #666;
            font-style: italic;
        }}
    </style>
</head>
<body>
    <div class="container">
        <h1>{}</h1>
        <p>Interactive demo of Go functions compiled to WebAssembly</p>
        
        <div id="loading" class="loading">Loading WebAssembly module...</div>
        <div id="content" style="display: none;">
{}        </div>
    </div>
    
    <script type="module" src="main.js"></script>
</body>
</html>"#,
            project_name, project_name, function_demos
        )
    }

    fn generate_function_demos(&self) -> String {
        self.exported_functions.iter().map(|func| {
            let inputs = (0..func.param_count)
                .map(|i| {
                    let param_name = if let Some(name) = func.param_names.get(i) {
                        name.as_str()
                    } else {
                        "param"
                    };
                    format!(r#"            <input type="number" id="{}_param_{}" placeholder="{}" value="{}">"#, 
                           func.export_name, i, param_name, if i < 2 { (i + 1) * 5 } else { 1 })
                })
                .collect::<Vec<_>>()
                .join("\n");

            format!(r#"            <div class="function-demo">
                <div class="function-title">{export_name}({param_signature})</div>
{inputs}
                <button onclick="call_{export_name}()">Call {export_name}</button>
                <div id="{export_name}_result" class="result" style="display: none;"></div>
            </div>"#,
                export_name = func.export_name,
                param_signature = func.param_names.join(", "),
                inputs = inputs
            )
        }).collect::<Vec<_>>().join("\n")
    }

    pub fn generate_javascript(&self, wasm_filename: &str) -> String {
        let function_implementations = self
            .exported_functions
            .iter()
            .map(|func| {
                let param_list = (0..func.param_count)
                    .map(|i| {
                        format!(
                            "parseInt(document.getElementById('{}_param_{}').value)",
                            func.export_name, i
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                format!(
                    r#"window.call_{export_name} = function() {{
    try {{
        const params = [{param_list}];
        
        // Validate inputs
        for (let i = 0; i < params.length; i++) {{
            if (isNaN(params[i])) {{
                showResult('{export_name}', 'Error: Invalid number in parameter ' + (i + 1), true);
                return;
            }}
        }}
        
        const result = wasmExports.{export_name}({param_list});
        showResult('{export_name}', `Result: ${{result}}`, false);
    }} catch (error) {{
        showResult('{export_name}', `Error: ${{error.message}}`, true);
    }}
}};"#,
                    export_name = func.export_name,
                    param_list = param_list
                )
            })
            .collect::<Vec<_>>()
            .join("\n\n");

        format!(
            r#"// Generated JavaScript bindings for Go WASM module
let wasmExports = {{}};

async function loadWasm() {{
    try {{
        const wasmModule = await WebAssembly.instantiateStreaming(fetch('{wasm_filename}'));
        wasmExports = wasmModule.instance.exports;
        
        console.log('WASM module loaded successfully');
        console.log('Available exports:', Object.keys(wasmExports));
        
        // Hide loading message and show content
        document.getElementById('loading').style.display = 'none';
        document.getElementById('content').style.display = 'block';
        
    }} catch (error) {{
        console.error('Error loading WASM:', error);
        document.getElementById('loading').innerHTML = `
            <div class="error">
                Error loading WebAssembly module: ${{error.message}}
                <br><br>
                Make sure the WASM file '{wasm_filename}' is in the same directory as this HTML file.
            </div>
        `;
    }}
}}

function showResult(functionName, message, isError) {{
    const resultDiv = document.getElementById(functionName + '_result');
    resultDiv.textContent = message;
    resultDiv.className = isError ? 'result error' : 'result';
    resultDiv.style.display = 'block';
}}

// Function implementations
{function_implementations}

// Load WASM when page loads
loadWasm();"#,
            wasm_filename = wasm_filename,
            function_implementations = function_implementations
        )
    }

    pub fn generate_typescript_definitions(&self) -> String {
        let function_definitions = self
            .exported_functions
            .iter()
            .map(|func| {
                let params = func
                    .param_names
                    .iter()
                    .map(|name| format!("{}: number", name))
                    .collect::<Vec<_>>()
                    .join(", ");

                let return_type = if func.has_return { "number" } else { "void" };

                format!("  {}: ({}) => {};", func.export_name, params, return_type)
            })
            .collect::<Vec<_>>()
            .join("\n");

        format!(
            r#"// TypeScript definitions for Go WASM module

export interface GoWasmExports {{
{function_definitions}
}}

export interface GoWasmModule {{
  instance: {{
    exports: GoWasmExports;
  }};
}}

declare global {{
  interface Window {{
{global_functions}  }}
}}

export function loadGoWasm(wasmPath: string): Promise<GoWasmModule>;"#,
            function_definitions = function_definitions,
            global_functions = self
                .exported_functions
                .iter()
                .map(|func| { format!("    call_{}: () => void;", func.export_name) })
                .collect::<Vec<_>>()
                .join("\n")
        )
    }

    pub fn generate_readme(&self, project_name: &str, wasm_filename: &str) -> String {
        let function_list = self
            .exported_functions
            .iter()
            .map(|func| {
                format!(
                    "- `{}({})` - {}",
                    func.export_name,
                    func.param_names.join(", "),
                    if func.has_return {
                        "returns number"
                    } else {
                        "no return value"
                    }
                )
            })
            .collect::<Vec<_>>()
            .join("\n");

        let export_names = self
            .exported_functions
            .iter()
            .map(|f| f.export_name.as_str())
            .collect::<Vec<_>>()
            .join(", ");

        let usage_examples = self
            .exported_functions
            .iter()
            .take(3)
            .map(|func| {
                let example_params = (0..func.param_count)
                    .map(|i| (i + 1) * 5)
                    .collect::<Vec<_>>();
                let params_str = example_params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("console.log({}({}));", func.export_name, params_str)
            })
            .collect::<Vec<_>>()
            .join("\n");

        format!(
            r#"# {project_name}

This project contains Go functions compiled to WebAssembly with automatically generated JavaScript bindings.

## Files

- `{wasm_filename}` - Compiled WebAssembly module
- `index.html` - Interactive demo page
- `main.js` - JavaScript bindings and demo logic
- `types.d.ts` - TypeScript definitions
- `README.md` - This file

## Available Functions

{function_list}

## Usage

### Web Browser

1. Serve the files from a web server (required for WASM loading):
   ```bash
   # Using Python
   python -m http.server 8000
   
   # Using Node.js
   npx serve .
   
   # Using any other static file server
   ```

2. Open `http://localhost:8000` in your browser

3. Use the interactive demo to test the functions

### Programmatic Usage

```javascript
// Load the WASM module
const wasmModule = await WebAssembly.instantiateStreaming(fetch('{wasm_filename}'));
const {{ {export_names} }} = wasmModule.instance.exports;

// Call the functions
{usage_examples}
```

### TypeScript Support

Import the type definitions for better development experience:

```typescript
import type {{ GoWasmExports }} from './types';

const wasmModule = await WebAssembly.instantiateStreaming(fetch('{wasm_filename}'));
const exports: GoWasmExports = wasmModule.instance.exports;
```

## Building

This project was generated by the Goiaba Go-to-WASM compiler.

To rebuild the WASM module:

```bash
goiaba source.go -o {wasm_filename} -w .
```

## Browser Compatibility

Requires a modern browser with WebAssembly support:
- Chrome 57+
- Firefox 52+  
- Safari 11+
- Edge 16+
"#,
            project_name = project_name,
            wasm_filename = wasm_filename,
            function_list = function_list,
            export_names = export_names,
            usage_examples = usage_examples
        )
    }
}

fn extract_export_name(source: &str, func_name: &str) -> Option<String> {
    let lines: Vec<&str> = source.lines().collect();

    for (i, line) in lines.iter().enumerate() {
        if line.contains(&format!("func {}", func_name)) {
            for j in (0..i).rev() {
                let check_line = lines[j].trim();
                if check_line.starts_with("//export ") {
                    let export_name = check_line.strip_prefix("//export ").unwrap().trim();
                    if !export_name.is_empty() {
                        return Some(export_name.to_string());
                    }
                }
                if !check_line.is_empty() && !check_line.starts_with("//") {
                    break;
                }
            }
            break;
        }
    }
    None
}
