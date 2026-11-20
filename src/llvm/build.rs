// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! Turns generated IR into an executable by driving a C compiler.

use super::runtime::RUNTIME_C;
use std::path::{Path, PathBuf};
use std::process::Command;

/// The C compiler to use: `$CC` when set, otherwise `clang`.
pub fn default_cc() -> String {
    std::env::var("CC").unwrap_or_else(|_| "clang".to_string())
}

/// A scratch directory that is removed when dropped.
struct Scratch(PathBuf);

impl Scratch {
    fn new() -> Result<Self, String> {
        let unique = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let dir = std::env::temp_dir().join(format!("goiaba-{}-{unique}", std::process::id()));
        std::fs::create_dir_all(&dir)
            .map_err(|e| format!("cannot create {}: {e}", dir.display()))?;
        Ok(Scratch(dir))
    }

    fn write(&self, name: &str, text: &str) -> Result<PathBuf, String> {
        let path = self.0.join(name);
        std::fs::write(&path, text).map_err(|e| format!("cannot write {}: {e}", path.display()))?;
        Ok(path)
    }
}

impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn quoted_value(line: &str) -> Option<String> {
    let start = line.find('"')? + 1;
    let end = line.rfind('"')?;
    (end > start).then(|| line[start..end].to_string())
}

/// Asks the C compiler which target triple and data layout it produces, so
/// the generated module can declare the same ones. `target` selects a
/// cross-compilation target; `None` means the compiler's default.
pub fn query_target(cc: &str, target: Option<&str>) -> Result<(String, String), String> {
    let scratch = Scratch::new()?;
    let probe = scratch.write("probe.c", "int goiaba_probe;\n")?;
    let mut cmd = Command::new(cc);
    if let Some(target) = target {
        cmd.arg(format!("--target={target}"));
    }
    let out = cmd
        .args(["-x", "c", "-S", "-emit-llvm", "-o", "-"])
        .arg(&probe)
        .output()
        .map_err(|e| format!("cannot run {cc}: {e}"))?;
    if !out.status.success() {
        return Err(format!(
            "{cc} failed:\n{}",
            String::from_utf8_lossy(&out.stderr)
        ));
    }
    let text = String::from_utf8_lossy(&out.stdout);
    let find = |prefix: &str| {
        text.lines()
            .find(|l| l.starts_with(prefix))
            .and_then(quoted_value)
            .ok_or_else(|| format!("{cc} did not report a {prefix}"))
    };
    Ok((find("target triple")?, find("target datalayout")?))
}

/// Compiles `ir` together with the goiaba runtime into an executable.
pub fn build_executable(
    cc: &str,
    ir: &str,
    output: &Path,
    extra_args: &[String],
) -> Result<(), String> {
    let scratch = Scratch::new()?;
    let module = scratch.write("main.ll", ir)?;
    let runtime = scratch.write("goiaba_runtime.c", RUNTIME_C)?;
    let out = Command::new(cc)
        .arg("-Wno-override-module")
        .args(extra_args)
        .arg(&module)
        .arg(&runtime)
        .arg("-o")
        .arg(output)
        .output()
        .map_err(|e| format!("cannot run {cc}: {e}"))?;
    if !out.status.success() {
        return Err(format!(
            "{cc} failed:\n{}",
            String::from_utf8_lossy(&out.stderr)
        ));
    }
    Ok(())
}
