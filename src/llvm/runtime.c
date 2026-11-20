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

#include <setjmp.h>
#include <stdarg.h>
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

// ---- panics, defer and recover ---------------------------------------------

typedef struct {
    const uint8_t *ptr;
    int64_t len;
} goiaba_string;

typedef struct {
    const uint8_t *name;
    int64_t name_len;
    void *fn;
} goiaba_method;

// Run-time type descriptor. The layout is shared with rtype_of in compiler.rs.
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

enum {
    GOIABA_KIND_BOOL = 1,
    GOIABA_KIND_INT = 2,
    GOIABA_KIND_UINT = 3,
    GOIABA_KIND_FLOAT = 4,
    GOIABA_KIND_STRING = 5,
    GOIABA_KIND_POINTER = 6,
    GOIABA_KIND_OTHER = 7,
    GOIABA_KIND_RUNTIME_ERROR = 8,
};
enum { GOIABA_FLAG_DIRECT = 1 };

// Defined by the generated module: the type of run-time error values, and a
// helper that calls a `func() string` method through the Go calling convention.
extern const goiaba_type goiaba_rterror_type;
extern void goiaba_call_string_method(void *fn, void *data, goiaba_string *out);

static void *goiaba_find_method(const goiaba_type *t, const char *name) {
    int64_t len = (int64_t)strlen(name);
    for (int64_t i = 0; i < t->nmethods; i++) {
        const goiaba_method *m = &t->methods[i];
        if (m->name_len == len && memcmp(m->name, name, (size_t)len) == 0) return m->fn;
    }
    return NULL;
}

// A deferred call. The compiler stores the call's operands after this header.
typedef struct goiaba_defer {
    struct goiaba_defer *next;
    void (*fn)(struct goiaba_defer *);
} goiaba_defer;

// One per active function that uses defer; lives in that function's frame.
typedef struct goiaba_frame {
    struct goiaba_frame *prev;
    goiaba_defer *defers;
    void *env; // jmp_buf to resume the function after a recovered panic
} goiaba_frame;

static goiaba_frame *goiaba_frames;

typedef struct goiaba_panic_state {
    const goiaba_type *type;
    void *data;
    int recovered;
    struct goiaba_panic_state *prev;
} goiaba_panic_state;

static goiaba_panic_state *goiaba_panics;

void goiaba_frame_enter(goiaba_frame *f, void *env) {
    f->prev = goiaba_frames;
    f->defers = NULL;
    f->env = env;
    goiaba_frames = f;
}

void goiaba_defer_push(goiaba_frame *f, goiaba_defer *d) {
    d->next = f->defers;
    f->defers = d;
}

// Runs the frame's deferred calls, last first, and removes the frame.
void goiaba_frame_exit(goiaba_frame *f) {
    while (f->defers != NULL) {
        goiaba_defer *d = f->defers;
        f->defers = d->next;
        d->fn(d);
    }
    goiaba_frames = f->prev;
}

// Prints a panic value the way Go does for an uncaught panic.
static void goiaba_print_any(const goiaba_type *t, void *data) {
    if (t == NULL) {
        fputs("nil", stderr);
        return;
    }
    void *method = goiaba_find_method(t, "Error");
    if (method == NULL) method = goiaba_find_method(t, "String");
    if (method != NULL) {
        goiaba_string s;
        goiaba_call_string_method(method, data, &s);
        goiaba_print_string(s.ptr, s.len);
        return;
    }
    switch (t->kind) {
    case GOIABA_KIND_BOOL:
        goiaba_print_bool(*(uint8_t *)data & 1);
        return;
    case GOIABA_KIND_INT:
        switch (t->size) {
        case 1: goiaba_print_int(*(int8_t *)data); return;
        case 2: goiaba_print_int(*(int16_t *)data); return;
        case 4: goiaba_print_int(*(int32_t *)data); return;
        default: goiaba_print_int(*(int64_t *)data); return;
        }
    case GOIABA_KIND_UINT:
        switch (t->size) {
        case 1: goiaba_print_uint(*(uint8_t *)data); return;
        case 2: goiaba_print_uint(*(uint16_t *)data); return;
        case 4: goiaba_print_uint(*(uint32_t *)data); return;
        default: goiaba_print_uint(*(uint64_t *)data); return;
        }
    case GOIABA_KIND_FLOAT:
        goiaba_print_float(t->size == 4 ? *(float *)data : *(double *)data);
        return;
    case GOIABA_KIND_STRING:
    case GOIABA_KIND_RUNTIME_ERROR: {
        goiaba_string *s = data;
        goiaba_print_string(s->ptr, s->len);
        return;
    }
    default:
        fputc('(', stderr);
        goiaba_print_string(t->name, t->name_len);
        fputs(") ", stderr);
        goiaba_print_pointer(data);
    }
}

// Starts a panic with the value (type, data). Deferred calls run, innermost
// frame first. If one of them recovers, control returns to the function that
// deferred it; otherwise the program prints the value and exits with status 2.
GOIABA_NORETURN void goiaba_panic(const goiaba_type *type, void *data) {
    goiaba_panic_state p = {type, data, 0, goiaba_panics};
    goiaba_panics = &p;
    while (goiaba_frames != NULL) {
        goiaba_frame *f = goiaba_frames;
        while (f->defers != NULL) {
            goiaba_defer *d = f->defers;
            f->defers = d->next;
            d->fn(d);
            if (p.recovered) {
                goiaba_panics = p.prev;
                longjmp(*(jmp_buf *)f->env, 1);
            }
        }
        goiaba_frames = f->prev;
    }
    fputs("panic: ", stderr);
    goiaba_print_any(type, data);
    fputs("\n\n", stderr);
    exit(2);
}

// The recover builtin: stops the current panic and stores its value in *out
// (an interface value, two words). Stores nil when nothing is panicking.
void goiaba_recover(void **out) {
    goiaba_panic_state *p = goiaba_panics;
    if (p == NULL || p->recovered) {
        out[0] = NULL;
        out[1] = NULL;
        return;
    }
    p->recovered = 1;
    out[0] = (void *)p->type;
    out[1] = p->data;
}

// Panics with a run-time error value whose message is the formatted text.
static GOIABA_NORETURN void goiaba_panic_message(const char *fmt, ...) {
    char buf[512];
    va_list args;
    va_start(args, fmt);
    int n = vsnprintf(buf, sizeof buf, fmt, args);
    va_end(args);
    if (n < 0) n = 0;
    if (n >= (int)sizeof buf) n = (int)sizeof buf - 1;
    uint8_t *text = goiaba_alloc(n);
    memcpy(text, buf, (size_t)n);
    goiaba_string *s = goiaba_alloc(sizeof *s);
    s->ptr = text;
    s->len = n;
    goiaba_panic(&goiaba_rterror_type, s);
}

GOIABA_NORETURN void goiaba_panic_runtime(const char *msg) {
    goiaba_panic_message("runtime error: %s", msg);
}

GOIABA_NORETURN void goiaba_panic_index(int64_t i, int64_t n) {
    goiaba_panic_message("runtime error: index out of range [%lld] with length %lld", (long long)i,
                         (long long)n);
}

GOIABA_NORETURN void goiaba_panic_slice(int64_t lo, int64_t hi, int64_t max) {
    goiaba_panic_message("runtime error: slice bounds out of range [%lld:%lld] with capacity %lld",
                         (long long)lo, (long long)hi, (long long)max);
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

static void *goiaba_find_method_n(const goiaba_type *t, const uint8_t *name, int64_t len) {
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
        void *fn = goiaba_find_method_n(have, want->methods[i].name, want->methods[i].name_len);
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
        goiaba_panic_message("runtime error: comparing uncomparable type %.*s", (int)t1->name_len,
                             (const char *)t1->name);
    }
    return t1->equal(d1, d2);
}

// A failed type assertion x.(T). `have` is x's dynamic type, NULL for nil.
GOIABA_NORETURN void goiaba_panic_assert(const goiaba_type *have, const uint8_t *iface,
                                         int64_t iface_len, const uint8_t *want, int64_t want_len,
                                         int32_t want_is_iface) {
    if (have != NULL && want_is_iface) {
        goiaba_panic_message("interface conversion: %.*s is not %.*s: missing method",
                             (int)have->name_len, (const char *)have->name, (int)want_len,
                             (const char *)want);
    }
    goiaba_panic_message("interface conversion: %.*s is %.*s, not %.*s", (int)iface_len,
                         (const char *)iface, have == NULL ? 3 : (int)have->name_len,
                         have == NULL ? "nil" : (const char *)have->name, (int)want_len,
                         (const char *)want);
}
