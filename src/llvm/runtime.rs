// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! The support code every module carries: libc declarations and a handful of
//! helpers written directly in LLVM IR. All helpers are `internal`, so LLVM
//! drops the ones a program does not use.
//!
//! There is no garbage collector yet: heap memory comes from `calloc` and is
//! never freed.

use std::fmt::Write as _;

/// Escapes bytes for an LLVM `c"..."` string constant.
pub fn c_escape(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len());
    for &b in bytes {
        if (0x20..0x7f).contains(&b) && b != b'"' && b != b'\\' {
            out.push(b as char);
        } else {
            let _ = write!(out, "\\{b:02X}");
        }
    }
    out
}

const MESSAGES: &[(&str, &str)] = &[
    (
        "goiaba.msg.index",
        "panic: runtime error: index out of range [%lld] with length %lld\n\n",
    ),
    (
        "goiaba.msg.slice",
        "panic: runtime error: slice bounds out of range [%lld:%lld] with capacity %lld\n\n",
    ),
    ("goiaba.msg.runtime", "panic: runtime error: %s\n\n"),
    ("goiaba.true", "true"),
    ("goiaba.false", "false"),
];

const HELPERS: &str = r#"
declare ptr @malloc(i64)
declare ptr @calloc(i64, i64)
declare i32 @memcmp(ptr, ptr, i64)
declare i32 @dprintf(i32, ptr, ...)
declare void @exit(i32) noreturn
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)

define internal void @goiaba.panic_if(i1 %cond, ptr %msg) {
entry:
  br i1 %cond, label %fail, label %ok
fail:
  %r = call i32 (i32, ptr, ...) @dprintf(i32 2, ptr @goiaba.msg.runtime, ptr %msg)
  call void @exit(i32 2)
  unreachable
ok:
  ret void
}

define internal void @goiaba.bounds(i64 %i, i64 %n) {
entry:
  %bad = icmp uge i64 %i, %n
  br i1 %bad, label %fail, label %ok
fail:
  %r = call i32 (i32, ptr, ...) @dprintf(i32 2, ptr @goiaba.msg.index, i64 %i, i64 %n)
  call void @exit(i32 2)
  unreachable
ok:
  ret void
}

define internal void @goiaba.slicecheck(i64 %lo, i64 %hi, i64 %max) {
entry:
  %a = icmp ugt i64 %hi, %max
  %b = icmp ugt i64 %lo, %hi
  %bad = or i1 %a, %b
  br i1 %bad, label %fail, label %ok
fail:
  %r = call i32 (i32, ptr, ...) @dprintf(i32 2, ptr @goiaba.msg.slice, i64 %lo, i64 %hi, i64 %max)
  call void @exit(i32 2)
  unreachable
ok:
  ret void
}

define internal %string @goiaba.concat(%string %a, %string %b) {
entry:
  %ap = extractvalue %string %a, 0
  %al = extractvalue %string %a, 1
  %bp = extractvalue %string %b, 0
  %bl = extractvalue %string %b, 1
  %n = add i64 %al, %bl
  %p = call ptr @malloc(i64 %n)
  call void @llvm.memcpy.p0.p0.i64(ptr %p, ptr %ap, i64 %al, i1 false)
  %q = getelementptr i8, ptr %p, i64 %al
  call void @llvm.memcpy.p0.p0.i64(ptr %q, ptr %bp, i64 %bl, i1 false)
  %r0 = insertvalue %string poison, ptr %p, 0
  %r1 = insertvalue %string %r0, i64 %n, 1
  ret %string %r1
}

define internal i32 @goiaba.strcmp(%string %a, %string %b) {
entry:
  %ap = extractvalue %string %a, 0
  %al = extractvalue %string %a, 1
  %bp = extractvalue %string %b, 0
  %bl = extractvalue %string %b, 1
  %lt = icmp ult i64 %al, %bl
  %gt = icmp ugt i64 %al, %bl
  %m = select i1 %lt, i64 %al, i64 %bl
  %c = call i32 @memcmp(ptr %ap, ptr %bp, i64 %m)
  %nz = icmp ne i32 %c, 0
  %d0 = select i1 %gt, i32 1, i32 0
  %d = select i1 %lt, i32 -1, i32 %d0
  %r = select i1 %nz, i32 %c, i32 %d
  ret i32 %r
}

define internal %slice @goiaba.append(%slice %s, ptr %elem, i64 %size) {
entry:
  %data = extractvalue %slice %s, 0
  %len = extractvalue %slice %s, 1
  %cap = extractvalue %slice %s, 2
  %full = icmp eq i64 %len, %cap
  br i1 %full, label %grow, label %store
grow:
  %dbl = shl i64 %cap, 1
  %empty = icmp eq i64 %cap, 0
  %ncap = select i1 %empty, i64 4, i64 %dbl
  %ndata = call ptr @calloc(i64 %ncap, i64 %size)
  %obytes = mul i64 %len, %size
  call void @llvm.memcpy.p0.p0.i64(ptr %ndata, ptr %data, i64 %obytes, i1 false)
  br label %store
store:
  %d = phi ptr [ %data, %entry ], [ %ndata, %grow ]
  %c = phi i64 [ %cap, %entry ], [ %ncap, %grow ]
  %off = mul i64 %len, %size
  %dst = getelementptr i8, ptr %d, i64 %off
  call void @llvm.memcpy.p0.p0.i64(ptr %dst, ptr %elem, i64 %size, i1 false)
  %nlen = add i64 %len, 1
  %r0 = insertvalue %slice poison, ptr %d, 0
  %r1 = insertvalue %slice %r0, i64 %nlen, 1
  %r2 = insertvalue %slice %r1, i64 %c, 2
  ret %slice %r2
}
"#;

/// Returns the message constants, libc declarations and helper functions.
pub fn prelude() -> String {
    let mut out = String::new();
    for (name, text) in MESSAGES {
        let mut bytes = text.as_bytes().to_vec();
        bytes.push(0);
        let _ = writeln!(
            out,
            "@{name} = private unnamed_addr constant [{} x i8] c\"{}\"",
            bytes.len(),
            c_escape(&bytes)
        );
    }
    out.push_str(HELPERS);
    out
}
