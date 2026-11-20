// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

// goiaba runtime for the LLVM backend.
//
// The compiler emits calls to the `goiaba_*` functions below. Only scalars
// and pointers cross this boundary, never structs by value, so the calls do
// not depend on how a platform's C ABI passes aggregates.
//
// It needs a hosted C library: stdio, stdlib and string.

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(_MSC_VER)
#define GOIABA_NORETURN __declspec(noreturn)
#else
#define GOIABA_NORETURN __attribute__((noreturn))
#endif

// ---- memory ----------------------------------------------------------------

// Returns `size` zeroed bytes. There is no collector yet: memory is never freed.
void *goiaba_alloc(int64_t size) {
    void *p = calloc(1, size > 0 ? (size_t)size : 1);
    if (p == NULL) {
        fputs("fatal error: out of memory\n", stderr);
        exit(2);
    }
    return p;
}

// ---- printing (the print and println builtins write to standard error) ----

void goiaba_print_string(const uint8_t *p, int64_t n) {
    if (n > 0) fwrite(p, 1, (size_t)n, stderr);
}

void goiaba_print_int(int64_t v) { fprintf(stderr, "%lld", (long long)v); }

void goiaba_print_uint(uint64_t v) { fprintf(stderr, "%llu", (unsigned long long)v); }

void goiaba_print_bool(int32_t v) { fputs(v ? "true" : "false", stderr); }

void goiaba_print_pointer(const void *p) { fprintf(stderr, "0x%llx", (unsigned long long)(uintptr_t)p); }

// Same algorithm and output as printfloat in Go's runtime/print.go.
void goiaba_print_float(double v) {
    if (v != v) {
        fputs("NaN", stderr);
        return;
    }
    if (v + v == v && v > 0) {
        fputs("+Inf", stderr);
        return;
    }
    if (v + v == v && v < 0) {
        fputs("-Inf", stderr);
        return;
    }

    enum { N = 7 }; // digits printed
    char buf[N + 7];
    int e = 0;
    buf[0] = '+';
    if (v == 0) {
        if (1 / v < 0) buf[0] = '-';
    } else {
        if (v < 0) {
            v = -v;
            buf[0] = '-';
        }
        while (v >= 10) {
            e++;
            v /= 10;
        }
        while (v < 1) {
            e--;
            v *= 10;
        }
        double h = 5.0;
        for (int i = 0; i < N; i++) h /= 10;
        v += h;
        if (v >= 10) {
            e++;
            v /= 10;
        }
    }
    for (int i = 0; i < N; i++) {
        int s = (int)v;
        buf[i + 2] = (char)(s + '0');
        v -= s;
        v *= 10;
    }
    buf[1] = buf[2];
    buf[2] = '.';
    buf[N + 2] = 'e';
    buf[N + 3] = '+';
    if (e < 0) {
        e = -e;
        buf[N + 3] = '-';
    }
    buf[N + 4] = (char)(e / 100 + '0');
    buf[N + 5] = (char)((e / 10) % 10 + '0');
    buf[N + 6] = (char)(e % 10 + '0');
    fwrite(buf, 1, sizeof buf, stderr);
}

// ---- panics ----------------------------------------------------------------

// Ends a panic whose message has already been printed.
GOIABA_NORETURN void goiaba_panic_exit(void) {
    fputs("\n\n", stderr);
    exit(2);
}

void goiaba_panic_begin(void) { fputs("panic: ", stderr); }

GOIABA_NORETURN void goiaba_panic_runtime(const char *msg) {
    fprintf(stderr, "panic: runtime error: %s", msg);
    goiaba_panic_exit();
}

GOIABA_NORETURN void goiaba_panic_index(int64_t i, int64_t n) {
    fprintf(stderr, "panic: runtime error: index out of range [%lld] with length %lld", (long long)i,
            (long long)n);
    goiaba_panic_exit();
}

GOIABA_NORETURN void goiaba_panic_slice(int64_t lo, int64_t hi, int64_t max) {
    fprintf(stderr, "panic: runtime error: slice bounds out of range [%lld:%lld] with capacity %lld",
            (long long)lo, (long long)hi, (long long)max);
    goiaba_panic_exit();
}

GOIABA_NORETURN void goiaba_panic_nil(void) {
    goiaba_panic_runtime("invalid memory address or nil pointer dereference");
}

// ---- strings ---------------------------------------------------------------

// Decodes the UTF-8 sequence at p[i] and stores its byte length in *width.
// Invalid input decodes to U+FFFD with width 1, as in Go.
int32_t goiaba_decode_rune(const uint8_t *p, int64_t len, int64_t i, int64_t *width) {
    uint8_t c = p[i];
    int64_t left = len - i;
    *width = 1;
    if (c < 0x80) return c;

    int need;
    int32_t r, min;
    if ((c & 0xE0) == 0xC0) {
        need = 2;
        r = c & 0x1F;
        min = 0x80;
    } else if ((c & 0xF0) == 0xE0) {
        need = 3;
        r = c & 0x0F;
        min = 0x800;
    } else if ((c & 0xF8) == 0xF0) {
        need = 4;
        r = c & 0x07;
        min = 0x10000;
    } else {
        return 0xFFFD;
    }
    if (left < need) return 0xFFFD;
    for (int k = 1; k < need; k++) {
        uint8_t cc = p[i + k];
        if ((cc & 0xC0) != 0x80) return 0xFFFD;
        r = (r << 6) | (cc & 0x3F);
    }
    if (r < min || r > 0x10FFFF || (r >= 0xD800 && r <= 0xDFFF)) return 0xFFFD;
    *width = need;
    return r;
}

// Writes the UTF-8 encoding of r to buf (at least 4 bytes) and returns its length.
int64_t goiaba_encode_rune(int32_t r, uint8_t *buf) {
    uint32_t c = (uint32_t)r;
    if (c > 0x10FFFF || (c >= 0xD800 && c <= 0xDFFF)) c = 0xFFFD;
    if (c < 0x80) {
        buf[0] = (uint8_t)c;
        return 1;
    }
    if (c < 0x800) {
        buf[0] = (uint8_t)(0xC0 | (c >> 6));
        buf[1] = (uint8_t)(0x80 | (c & 0x3F));
        return 2;
    }
    if (c < 0x10000) {
        buf[0] = (uint8_t)(0xE0 | (c >> 12));
        buf[1] = (uint8_t)(0x80 | ((c >> 6) & 0x3F));
        buf[2] = (uint8_t)(0x80 | (c & 0x3F));
        return 3;
    }
    buf[0] = (uint8_t)(0xF0 | (c >> 18));
    buf[1] = (uint8_t)(0x80 | ((c >> 12) & 0x3F));
    buf[2] = (uint8_t)(0x80 | ((c >> 6) & 0x3F));
    buf[3] = (uint8_t)(0x80 | (c & 0x3F));
    return 4;
}

// ---- interfaces --------------------------------------------------------------

// Layouts shared with the compiler (see rtype_of and iface_desc in compiler.rs).
typedef struct {
    const uint8_t *name;
    int64_t name_len;
    void *fn;
} goiaba_method;

typedef struct goiaba_type {
    int32_t kind;
    int32_t flags; // bit 0: the interface data word is the value itself
    int64_t size;
    const uint8_t *name;
    int64_t name_len;
    int32_t (*equal)(const void *, const void *);
    int64_t nmethods;
    const goiaba_method *methods; // sorted by name
} goiaba_type;

typedef struct {
    const uint8_t *name;
    int64_t name_len;
} goiaba_imethod;

typedef struct {
    int64_t nmethods;
    const goiaba_imethod *methods;
    const uint8_t *name;
    int64_t name_len;
} goiaba_iface_desc;

enum { GOIABA_FLAG_DIRECT = 1 };

static void *goiaba_find_method(const goiaba_type *t, const uint8_t *name, int64_t len) {
    for (int64_t i = 0; i < t->nmethods; i++) {
        const goiaba_method *m = &t->methods[i];
        if (m->name_len == len && memcmp(m->name, name, (size_t)len) == 0) return m->fn;
    }
    return NULL;
}

typedef struct goiaba_itab_entry {
    const goiaba_iface_desc *want;
    const goiaba_type *have;
    void **tab;
    struct goiaba_itab_entry *next;
} goiaba_itab_entry;

static goiaba_itab_entry *goiaba_itabs;

// Returns the method table that lets a value of dynamic type `have` be used
// as interface `want`, or NULL when the type lacks a method (or is nil).
// A table is { type, fn0, fn1, ... } in the interface's method order.
void *goiaba_itab(const goiaba_iface_desc *want, const goiaba_type *have) {
    if (have == NULL) return NULL;
    for (goiaba_itab_entry *e = goiaba_itabs; e != NULL; e = e->next) {
        if (e->want == want && e->have == have) return e->tab;
    }
    void **tab = goiaba_alloc((int64_t)sizeof(void *) * (want->nmethods + 1));
    tab[0] = (void *)have;
    for (int64_t i = 0; i < want->nmethods; i++) {
        void *fn = goiaba_find_method(have, want->methods[i].name, want->methods[i].name_len);
        if (fn == NULL) return NULL;
        tab[i + 1] = fn;
    }
    goiaba_itab_entry *e = goiaba_alloc(sizeof *e);
    e->want = want;
    e->have = have;
    e->tab = tab;
    e->next = goiaba_itabs;
    goiaba_itabs = e;
    return tab;
}

// Interface equality: same dynamic type and equal values.
int32_t goiaba_iface_eq(const goiaba_type *t1, const void *d1, const goiaba_type *t2,
                        const void *d2) {
    if (t1 != t2) return 0;
    if (t1 == NULL) return 1;
    if (t1->flags & GOIABA_FLAG_DIRECT) return d1 == d2;
    if (t1->equal == NULL) {
        fputs("panic: runtime error: comparing uncomparable type ", stderr);
        goiaba_print_string(t1->name, t1->name_len);
        goiaba_panic_exit();
    }
    return t1->equal(d1, d2);
}

// A failed type assertion x.(T). `have` is x's dynamic type, NULL for nil.
GOIABA_NORETURN void goiaba_panic_assert(const goiaba_type *have, const uint8_t *iface,
                                         int64_t iface_len, const uint8_t *want, int64_t want_len,
                                         int32_t want_is_iface) {
    fputs("panic: interface conversion: ", stderr);
    if (have != NULL && want_is_iface) {
        goiaba_print_string(have->name, have->name_len);
        fputs(" is not ", stderr);
        goiaba_print_string(want, want_len);
        fputs(": missing method", stderr);
    } else {
        goiaba_print_string(iface, iface_len);
        fputs(" is ", stderr);
        if (have == NULL) {
            fputs("nil", stderr);
        } else {
            goiaba_print_string(have->name, have->name_len);
        }
        fputs(", not ", stderr);
        goiaba_print_string(want, want_len);
    }
    goiaba_panic_exit();
}
