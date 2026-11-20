// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! Support code for generated modules.
//!
//! It has two halves. [`RUNTIME_C`] is a C file that must be compiled and
//! linked with the generated IR; it holds allocation, printing and panics.
//! The helpers below are written directly in LLVM IR and are emitted into
//! every module as `internal` functions, so that hot checks such as bounds
//! tests inline and unused helpers are dropped.

use std::fmt::Write as _;

/// The C half of the runtime. Compile it together with the generated IR,
/// for example `clang main.ll goiaba_runtime.c`.
pub const RUNTIME_C: &str = include_str!("runtime.c");

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

const HELPERS: &str = r#"
declare i32 @memcmp(ptr, ptr, i64)
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)

declare ptr @goiaba_alloc(i64)
declare void @goiaba_print_string(ptr, i64)
declare void @goiaba_print_int(i64)
declare void @goiaba_print_uint(i64)
declare void @goiaba_print_bool(i32)
declare void @goiaba_print_pointer(ptr)
declare void @goiaba_print_float(double)
declare void @goiaba_panic(ptr, ptr) noreturn
declare void @goiaba_recover(ptr)
declare void @goiaba_frame_enter(ptr, ptr)
declare void @goiaba_frame_exit(ptr)
declare void @goiaba_defer_push(ptr, ptr)
declare i32 @_setjmp(ptr) returns_twice
declare void @goiaba_panic_runtime(ptr) noreturn
declare void @goiaba_panic_index(i64, i64) noreturn
declare void @goiaba_panic_slice(i64, i64, i64) noreturn
declare void @goiaba_panic_nil() noreturn
declare i32 @goiaba_decode_rune(ptr, i64, i64, ptr)
declare i64 @goiaba_encode_rune(i32, ptr)
declare ptr @goiaba_itab(ptr, ptr)
declare ptr @goiaba_map_new(ptr, i64, i64)
declare i64 @goiaba_map_len(ptr)
declare ptr @goiaba_map_get(ptr, ptr)
declare ptr @goiaba_map_assign(ptr, ptr)
declare void @goiaba_map_delete(ptr, ptr)
declare i32 @goiaba_map_next(ptr, ptr, ptr, ptr)
declare i32 @goiaba_iface_eq(ptr, ptr, ptr, ptr)
declare void @goiaba_panic_assert(ptr, ptr, i64, ptr, i64, i32) noreturn

; The dynamic type behind a non-empty interface's method table (null for nil).
define internal ptr @goiaba.itab_type(ptr %tab) {
entry:
  %nil = icmp eq ptr %tab, null
  br i1 %nil, label %done, label %read
read:
  %t = load ptr, ptr %tab
  br label %done
done:
  %r = phi ptr [ null, %entry ], [ %t, %read ]
  ret ptr %r
}

define internal void @goiaba.assert_ok(i1 %ok, ptr %have, ptr %iface, i64 %ilen, ptr %want, i64 %wlen, i32 %wiface) {
entry:
  br i1 %ok, label %done, label %fail
fail:
  call void @goiaba_panic_assert(ptr %have, ptr %iface, i64 %ilen, ptr %want, i64 %wlen, i32 %wiface)
  unreachable
done:
  ret void
}

; The type of run-time error values such as an out-of-range index. Its data
; is a string holding the whole message, and it implements error.
@goiaba.rterror.name = private constant [13 x i8] c"runtime.Error"
@goiaba.rterror.method = private constant [5 x i8] c"Error"
@goiaba.rterror.methods = private constant [1 x %rmethod] [%rmethod { ptr @goiaba.rterror.method, i64 5, ptr @goiaba.rterror.Error }]
@goiaba_rterror_type = constant %rtype { i32 8, i32 0, i64 16, ptr @goiaba.rterror.name, i64 13, ptr null, i64 1, ptr @goiaba.rterror.methods }

define internal %string @goiaba.rterror.Error(ptr %data) {
entry:
  %s = load %string, ptr %data
  ret %string %s
}

; Lets the C runtime call a `func() string` method on an interface's data.
define void @goiaba_call_string_method(ptr %fn, ptr %data, ptr %out) {
entry:
  %s = call %string %fn(ptr %data)
  store %string %s, ptr %out
  ret void
}

define internal void @goiaba.panic_if(i1 %cond, ptr %msg) {
entry:
  br i1 %cond, label %fail, label %ok
fail:
  call void @goiaba_panic_runtime(ptr %msg)
  unreachable
ok:
  ret void
}

define internal void @goiaba.nilcheck(ptr %p) {
entry:
  %bad = icmp eq ptr %p, null
  br i1 %bad, label %fail, label %ok
fail:
  call void @goiaba_panic_nil()
  unreachable
ok:
  ret void
}

define internal void @goiaba.bounds(i64 %i, i64 %n) {
entry:
  %bad = icmp uge i64 %i, %n
  br i1 %bad, label %fail, label %ok
fail:
  call void @goiaba_panic_index(i64 %i, i64 %n)
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
  call void @goiaba_panic_slice(i64 %lo, i64 %hi, i64 %max)
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
  %p = call ptr @goiaba_alloc(i64 %n)
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

; Copies n bytes into a fresh allocation: the body of string <-> []byte conversions.
define internal ptr @goiaba.clone(ptr %p, i64 %n) {
entry:
  %q = call ptr @goiaba_alloc(i64 %n)
  call void @llvm.memcpy.p0.p0.i64(ptr %q, ptr %p, i64 %n, i1 false)
  ret ptr %q
}

; Appends n elements of `size` bytes each, growing the backing array when needed.
define internal %slice @goiaba.append(%slice %s, ptr %elems, i64 %n, i64 %size) {
entry:
  %data = extractvalue %slice %s, 0
  %len = extractvalue %slice %s, 1
  %cap = extractvalue %slice %s, 2
  %nlen = add i64 %len, %n
  %fits = icmp ule i64 %nlen, %cap
  br i1 %fits, label %store, label %grow
grow:
  %dbl = shl i64 %cap, 1
  %big = icmp ugt i64 %nlen, %dbl
  %want = select i1 %big, i64 %nlen, i64 %dbl
  %small = icmp ult i64 %want, 4
  %ncap = select i1 %small, i64 4, i64 %want
  %bytes = mul i64 %ncap, %size
  %ndata = call ptr @goiaba_alloc(i64 %bytes)
  %obytes = mul i64 %len, %size
  call void @llvm.memcpy.p0.p0.i64(ptr %ndata, ptr %data, i64 %obytes, i1 false)
  br label %store
store:
  %d = phi ptr [ %data, %entry ], [ %ndata, %grow ]
  %c = phi i64 [ %cap, %entry ], [ %ncap, %grow ]
  %off = mul i64 %len, %size
  %dst = getelementptr i8, ptr %d, i64 %off
  %abytes = mul i64 %n, %size
  call void @llvm.memcpy.p0.p0.i64(ptr %dst, ptr %elems, i64 %abytes, i1 false)
  %r0 = insertvalue %slice poison, ptr %d, 0
  %r1 = insertvalue %slice %r0, i64 %nlen, 1
  %r2 = insertvalue %slice %r1, i64 %c, 2
  ret %slice %r2
}
"#;

/// Returns the runtime declarations and the IR helper functions.
pub fn prelude() -> &'static str {
    HELPERS
}
