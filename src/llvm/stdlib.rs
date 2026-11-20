// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! The standard-library packages goiaba can import.
//!
//! They are a small subset of Go's, written in Go under `std/` and compiled
//! from source together with the program. Functions declared there without a
//! body are implemented in the C runtime as `goiaba_<package>_<name>`.

/// Returns the source of the package with the given import path.
pub fn source(path: &str) -> Option<&'static str> {
    Some(match path {
        "errors" => include_str!("std/errors.go"),
        "fmt" => include_str!("std/fmt.go"),
        "io" => include_str!("std/io.go"),
        "math" => include_str!("std/math.go"),
        "os" => include_str!("std/os.go"),
        "runtime" => include_str!("std/runtime.go"),
        "sort" => include_str!("std/sort.go"),
        "strconv" => include_str!("std/strconv.go"),
        "strings" => include_str!("std/strings.go"),
        "sync" => include_str!("std/sync.go"),
        "sync/atomic" => include_str!("std/atomic.go"),
        "time" => include_str!("std/time.go"),
        "unicode" => include_str!("std/unicode.go"),
        "unicode/utf8" => include_str!("std/utf8.go"),
        _ => return None,
    })
}
