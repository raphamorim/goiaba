// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! LLVM backend: lowers the Go AST to textual LLVM IR (`.ll`).
//!
//! The IR is emitted as text, so goiaba itself does not link against LLVM.
//! Feed the result to `clang`, `llc` or `lli` to get native code.

pub mod compiler;
mod runtime;

pub use compiler::compile_str;
