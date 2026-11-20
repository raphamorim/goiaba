// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! LLVM backend: lowers the Go AST to textual LLVM IR (`.ll`).
//!
//! The IR is emitted as text, so goiaba itself does not link against LLVM.
//! The result must be linked with the C runtime in [`RUNTIME_C`]; the
//! [`build`] module does that by driving `clang`.

pub mod build;
pub mod compiler;
mod runtime;
mod stdlib;

pub use compiler::{Options, compile_sources, compile_str};
pub use runtime::RUNTIME_C;
