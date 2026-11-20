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

#if defined(_WIN32)
#include <io.h>
#define goiaba_fd_write _write
#else
#include <unistd.h>
#define goiaba_fd_write write
#endif

#if defined(_MSC_VER)
#define GOIABA_NORETURN __declspec(noreturn)
#else
#define GOIABA_NORETURN __attribute__((noreturn))
#endif

// ---- memory: a conservative mark-sweep garbage collector ---------------------
//
// The heap is a set of chunks cut into 16-byte blocks. An object is a head
// block followed by tail blocks; one state byte per block records which.
// Collection is stop-the-world and conservative: every word on the stack, in
// the registers, in the Go globals and inside a live object is treated as a
// possible pointer, and a pointer anywhere into an object keeps it alive.
// Nothing moves, so C code may hold Go pointers across a collection as long
// as they are on its stack.
//
// Known limit: an object is kept alive only by a pointer into it. If the
// optimizer keeps nothing but an address outside the object (for example one
// past its end), the object can be freed too early.

enum { GOIABA_BLOCK = 16, GOIABA_CHUNK_BLOCKS = 1 << 16 };
enum { GOIABA_FREE = 0, GOIABA_HEAD = 1, GOIABA_TAIL = 2, GOIABA_MARKED = 3 };

typedef struct {
    uint8_t *base;
    int64_t nblocks;
    uint8_t *state; // one of GOIABA_FREE/HEAD/TAIL/MARKED per block
    int64_t hint;   // where the next search for free blocks starts
    int64_t used;   // blocks in use
} goiaba_chunk;

typedef struct {
    void *addr;
    int64_t size;
} goiaba_root;

// Emitted by the compiler: the address and size of every Go global variable.
extern const goiaba_root goiaba_roots[];
extern const int64_t goiaba_nroots;

static goiaba_chunk *goiaba_chunks; // sorted by base address
static int64_t goiaba_nchunks, goiaba_chunks_cap;
static int64_t goiaba_cursor;       // chunk where the last allocation succeeded
static uint8_t *goiaba_stack_base;  // highest stack address to scan; NULL disables collection
static int goiaba_gc_enabled = 1, goiaba_gc_trace;
static int64_t goiaba_live_bytes;   // bytes in use after the last collection
static int64_t goiaba_since_gc;     // bytes allocated since then
static uint64_t goiaba_total_alloc; // bytes allocated over the program's life
static uint32_t goiaba_num_gc;

// Work list of objects whose contents still have to be scanned.
static uint8_t **goiaba_marks;
static int64_t goiaba_nmarks, goiaba_marks_cap;

// Extra root ranges inside the runtime itself (see goiaba_gc_roots).
static void goiaba_scan(const uint8_t *lo, const uint8_t *hi);
static void goiaba_scan_runtime_roots(void);

static void goiaba_fatal(const char *msg) {
    fputs("fatal error: ", stderr);
    fputs(msg, stderr);
    fputc('\n', stderr);
    exit(2);
}

// Finds the chunk containing p, or NULL.
static goiaba_chunk *goiaba_chunk_of(const uint8_t *p) {
    int64_t lo = 0, hi = goiaba_nchunks;
    while (lo < hi) {
        int64_t mid = (lo + hi) / 2;
        goiaba_chunk *c = &goiaba_chunks[mid];
        if (p < c->base) {
            hi = mid;
        } else if (p >= c->base + c->nblocks * GOIABA_BLOCK) {
            lo = mid + 1;
        } else {
            return c;
        }
    }
    return NULL;
}

static goiaba_chunk *goiaba_new_chunk(int64_t nblocks) {
    if (nblocks < GOIABA_CHUNK_BLOCKS) nblocks = GOIABA_CHUNK_BLOCKS;
    uint8_t *base = calloc((size_t)nblocks, GOIABA_BLOCK);
    uint8_t *state = calloc((size_t)nblocks, 1);
    if (base == NULL || state == NULL) goiaba_fatal("out of memory");
    if (goiaba_nchunks == goiaba_chunks_cap) {
        goiaba_chunks_cap = goiaba_chunks_cap ? goiaba_chunks_cap * 2 : 16;
        goiaba_chunks = realloc(goiaba_chunks, (size_t)goiaba_chunks_cap * sizeof *goiaba_chunks);
        if (goiaba_chunks == NULL) goiaba_fatal("out of memory");
    }
    int64_t at = goiaba_nchunks;
    while (at > 0 && goiaba_chunks[at - 1].base > base) {
        goiaba_chunks[at] = goiaba_chunks[at - 1];
        at--;
    }
    goiaba_chunks[at] = (goiaba_chunk){base, nblocks, state, 0, 0};
    goiaba_nchunks++;
    goiaba_cursor = at;
    return &goiaba_chunks[at];
}

// Takes `n` consecutive free blocks from the chunk, or returns NULL.
static uint8_t *goiaba_take(goiaba_chunk *c, int64_t n) {
    if (c->nblocks - c->used < n) return NULL;
    for (int pass = 0; pass < 2; pass++) {
        int64_t from = pass == 0 ? c->hint : 0;
        int64_t to = pass == 0 ? c->nblocks : c->hint + n;
        if (to > c->nblocks) to = c->nblocks;
        int64_t run = 0;
        for (int64_t i = from; i < to; i++) {
            run = c->state[i] == GOIABA_FREE ? run + 1 : 0;
            if (run < n) continue;
            int64_t start = i - n + 1;
            c->state[start] = GOIABA_HEAD;
            memset(c->state + start + 1, GOIABA_TAIL, (size_t)(n - 1));
            c->hint = i + 1;
            c->used += n;
            return c->base + start * GOIABA_BLOCK;
        }
    }
    return NULL;
}

// Marks the object that p points into, if any, and queues it for scanning.
static void goiaba_mark(const uint8_t *p) {
    goiaba_chunk *c = goiaba_chunk_of(p);
    if (c == NULL) return;
    int64_t i = (p - c->base) / GOIABA_BLOCK;
    while (c->state[i] == GOIABA_TAIL) i--;
    if (c->state[i] != GOIABA_HEAD) return; // free, or already marked
    c->state[i] = GOIABA_MARKED;
    if (goiaba_nmarks == goiaba_marks_cap) {
        goiaba_marks_cap = goiaba_marks_cap ? goiaba_marks_cap * 2 : 1024;
        goiaba_marks = realloc(goiaba_marks, (size_t)goiaba_marks_cap * sizeof *goiaba_marks);
        if (goiaba_marks == NULL) goiaba_fatal("out of memory");
    }
    goiaba_marks[goiaba_nmarks++] = c->base + i * GOIABA_BLOCK;
}

// Treats every aligned word in [lo, hi) as a possible pointer.
static void goiaba_scan(const uint8_t *lo, const uint8_t *hi) {
    lo = (const uint8_t *)(((uintptr_t)lo + sizeof(void *) - 1) & ~(uintptr_t)(sizeof(void *) - 1));
    for (; lo + sizeof(void *) <= hi; lo += sizeof(void *)) {
        const uint8_t *p;
        memcpy(&p, lo, sizeof p);
        if (p != NULL) goiaba_mark(p);
    }
}

static void goiaba_drain_marks(void) {
    while (goiaba_nmarks > 0) {
        uint8_t *obj = goiaba_marks[--goiaba_nmarks];
        goiaba_chunk *c = goiaba_chunk_of(obj);
        int64_t i = (obj - c->base) / GOIABA_BLOCK, end = i + 1;
        while (end < c->nblocks && c->state[end] == GOIABA_TAIL) end++;
        goiaba_scan(obj, c->base + end * GOIABA_BLOCK);
    }
}

// Frees every unmarked object and returns the bytes still in use.
static int64_t goiaba_sweep(void) {
    int64_t live = 0, kept = 0;
    for (int64_t k = 0; k < goiaba_nchunks; k++) {
        goiaba_chunk *c = &goiaba_chunks[k];
        int keep = 0;
        c->used = 0;
        for (int64_t i = 0; i < c->nblocks; i++) {
            switch (c->state[i]) {
            case GOIABA_MARKED:
                c->state[i] = GOIABA_HEAD;
                keep = 1;
                c->used++;
                break;
            case GOIABA_HEAD:
                c->state[i] = GOIABA_FREE;
                keep = 0;
                break;
            case GOIABA_TAIL:
                if (keep) {
                    c->used++;
                } else {
                    c->state[i] = GOIABA_FREE;
                }
                break;
            default:
                keep = 0;
            }
        }
        c->hint = 0;
        live += c->used * GOIABA_BLOCK;
        // Give an empty oversized chunk back; ordinary chunks are reused.
        if (c->used == 0 && c->nblocks > GOIABA_CHUNK_BLOCKS) {
            free(c->base);
            free(c->state);
            continue;
        }
        goiaba_chunks[kept++] = *c;
    }
    goiaba_nchunks = kept;
    goiaba_cursor = 0;
    return live;
}

// Runs one collection. Must not be inlined: its frame marks the low end of
// the stack range that is scanned.
#if defined(__GNUC__)
__attribute__((noinline))
#endif
void goiaba_collect(void) {
    if (goiaba_stack_base == NULL) return;
    // setjmp copies the callee-saved registers into `env`, on this stack.
    jmp_buf env;
    setjmp(env);
    goiaba_scan((const uint8_t *)&env, goiaba_stack_base);
    for (int64_t i = 0; i < goiaba_nroots; i++) {
        const uint8_t *addr = goiaba_roots[i].addr;
        goiaba_scan(addr, addr + goiaba_roots[i].size);
    }
    goiaba_scan_runtime_roots();
    goiaba_drain_marks();
    int64_t before = goiaba_live_bytes + goiaba_since_gc;
    goiaba_live_bytes = goiaba_sweep();
    goiaba_since_gc = 0;
    goiaba_num_gc++;
    if (goiaba_gc_trace) {
        fprintf(stderr, "gc %u: %lld KB -> %lld KB, %lld chunks\n", goiaba_num_gc,
                (long long)(before / 1024), (long long)(goiaba_live_bytes / 1024),
                (long long)goiaba_nchunks);
    }
}

// Returns `size` zeroed bytes of garbage-collected memory.
void *goiaba_alloc(int64_t size) {
    if (size < 0) goiaba_fatal("negative allocation size");
    int64_t n = size == 0 ? 1 : (size + GOIABA_BLOCK - 1) / GOIABA_BLOCK;
    int64_t bytes = n * GOIABA_BLOCK;
    // Collect once the heap has doubled since the last collection.
    int64_t trigger = goiaba_live_bytes < (4 << 20) ? (4 << 20) : goiaba_live_bytes;
    if (goiaba_gc_enabled && goiaba_since_gc >= trigger) goiaba_collect();

    uint8_t *p = NULL;
    for (int64_t k = 0; k < goiaba_nchunks && p == NULL; k++) {
        int64_t at = (goiaba_cursor + k) % goiaba_nchunks;
        p = goiaba_take(&goiaba_chunks[at], n);
        if (p != NULL) goiaba_cursor = at;
    }
    if (p == NULL) p = goiaba_take(goiaba_new_chunk(n), n);
    memset(p, 0, (size_t)bytes);
    goiaba_since_gc += bytes;
    goiaba_total_alloc += (uint64_t)bytes;
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
    const struct goiaba_type *elem; // element, pointee or map value type
    int64_t count;                  // array length or number of struct fields
    const void *extra;              // struct: goiaba_field[]; map: the key type
} goiaba_type;

typedef struct {
    const uint8_t *name;
    int64_t name_len;
    const goiaba_type *type;
    int64_t offset;
} goiaba_field;

enum {
    GOIABA_KIND_BOOL = 1,
    GOIABA_KIND_INT = 2,
    GOIABA_KIND_UINT = 3,
    GOIABA_KIND_FLOAT = 4,
    GOIABA_KIND_STRING = 5,
    GOIABA_KIND_POINTER = 6,
    GOIABA_KIND_OTHER = 7,
    GOIABA_KIND_RUNTIME_ERROR = 8,
    GOIABA_KIND_SLICE = 9,
    GOIABA_KIND_ARRAY = 10,
    GOIABA_KIND_STRUCT = 11,
    GOIABA_KIND_MAP = 12,
    GOIABA_KIND_INTERFACE = 13,
    GOIABA_KIND_FUNC = 14,
};
enum {
    GOIABA_FLAG_DIRECT = 1,  // the interface data word is the value itself
    GOIABA_FLAG_METHODS = 2, // an interface type that has methods
};

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

// ---- maps --------------------------------------------------------------------

// A hash table that keeps entries in insertion order. `buckets` maps a hash
// to an entry index; deleted entries stay as tombstones until the next grow.
// Keys are compared as raw bytes, except strings, which compare by content.
typedef struct goiaba_map {
    int64_t count;    // live entries
    int64_t used;     // entries including tombstones
    int64_t cap;      // entries allocated
    int64_t nbuckets; // power of two
    int64_t *buckets; // entry index + 1, or 0 when empty
    uint8_t *entries;
    int64_t keysize, valsize, valoff, entsize;
    int32_t string_key;
} goiaba_map;

enum { GOIABA_MAP_HASH = 0, GOIABA_MAP_LIVE = 8, GOIABA_MAP_KEY = 16 };

static int64_t goiaba_round8(int64_t n) { return (n + 7) & ~(int64_t)7; }

static uint64_t goiaba_hash_bytes(const uint8_t *p, int64_t n) {
    uint64_t h = 1469598103934665603ULL; // FNV-1a
    for (int64_t i = 0; i < n; i++) {
        h ^= p[i];
        h *= 1099511628211ULL;
    }
    return h;
}

static uint64_t goiaba_map_hash(const goiaba_map *m, const void *key) {
    if (m->string_key) {
        const goiaba_string *s = key;
        return goiaba_hash_bytes(s->ptr, s->len);
    }
    return goiaba_hash_bytes(key, m->keysize);
}

static int goiaba_map_key_eq(const goiaba_map *m, const void *a, const void *b) {
    if (m->string_key) {
        const goiaba_string *x = a, *y = b;
        return x->len == y->len && memcmp(x->ptr, y->ptr, (size_t)x->len) == 0;
    }
    return memcmp(a, b, (size_t)m->keysize) == 0;
}

static uint8_t *goiaba_map_entry(const goiaba_map *m, int64_t i) { return m->entries + i * m->entsize; }

void *goiaba_map_new(const goiaba_type *key, int64_t valsize, int64_t hint) {
    (void)hint;
    goiaba_map *m = goiaba_alloc(sizeof *m);
    m->keysize = key->size;
    m->valsize = valsize;
    m->valoff = GOIABA_MAP_KEY + goiaba_round8(key->size);
    m->entsize = m->valoff + goiaba_round8(valsize);
    m->string_key = key->kind == GOIABA_KIND_STRING;
    return m;
}

int64_t goiaba_map_len(const goiaba_map *m) { return m == NULL ? 0 : m->count; }

// Returns the index of the entry holding `key`, or -1.
static int64_t goiaba_map_find(const goiaba_map *m, const void *key, uint64_t hash) {
    if (m->nbuckets == 0) return -1;
    uint64_t mask = (uint64_t)m->nbuckets - 1;
    for (uint64_t b = hash & mask; m->buckets[b] != 0; b = (b + 1) & mask) {
        int64_t i = m->buckets[b] - 1;
        uint8_t *e = goiaba_map_entry(m, i);
        uint64_t h;
        memcpy(&h, e + GOIABA_MAP_HASH, sizeof h);
        if (e[GOIABA_MAP_LIVE] && h == hash && goiaba_map_key_eq(m, e + GOIABA_MAP_KEY, key)) return i;
    }
    return -1;
}

static void goiaba_map_link(goiaba_map *m, int64_t i, uint64_t hash) {
    uint64_t mask = (uint64_t)m->nbuckets - 1;
    uint64_t b = hash & mask;
    while (m->buckets[b] != 0) b = (b + 1) & mask;
    m->buckets[b] = i + 1;
}

// Makes room for more entries, dropping tombstones.
static void goiaba_map_grow(goiaba_map *m) {
    int64_t cap = m->count * 2 < 8 ? 8 : m->count * 2;
    int64_t nbuckets = 16;
    while (nbuckets < cap * 2) nbuckets *= 2;
    uint8_t *entries = goiaba_alloc(cap * m->entsize);
    int64_t *buckets = goiaba_alloc(nbuckets * (int64_t)sizeof(int64_t));
    uint8_t *old = m->entries;
    int64_t old_used = m->used;
    m->entries = entries;
    m->buckets = buckets;
    m->cap = cap;
    m->nbuckets = nbuckets;
    m->used = 0;
    for (int64_t i = 0; i < old_used; i++) {
        uint8_t *e = old + i * m->entsize;
        if (!e[GOIABA_MAP_LIVE]) continue;
        memcpy(goiaba_map_entry(m, m->used), e, (size_t)m->entsize);
        uint64_t h;
        memcpy(&h, e + GOIABA_MAP_HASH, sizeof h);
        goiaba_map_link(m, m->used, h);
        m->used++;
    }
}

// m[key] as a value: a pointer to the stored value, or NULL when absent.
void *goiaba_map_get(const goiaba_map *m, const void *key) {
    if (m == NULL || m->count == 0) return NULL;
    int64_t i = goiaba_map_find(m, key, goiaba_map_hash(m, key));
    return i < 0 ? NULL : goiaba_map_entry(m, i) + m->valoff;
}

// m[key] as an assignment target: the value slot, inserted zeroed if needed.
void *goiaba_map_assign(goiaba_map *m, const void *key) {
    if (m == NULL) goiaba_panic_message("assignment to entry in nil map");
    uint64_t hash = goiaba_map_hash(m, key);
    int64_t i = goiaba_map_find(m, key, hash);
    if (i >= 0) return goiaba_map_entry(m, i) + m->valoff;
    if (m->used == m->cap || (m->used + 1) * 4 > m->nbuckets * 3) goiaba_map_grow(m);
    i = m->used++;
    uint8_t *e = goiaba_map_entry(m, i);
    memcpy(e + GOIABA_MAP_HASH, &hash, sizeof hash);
    e[GOIABA_MAP_LIVE] = 1;
    memcpy(e + GOIABA_MAP_KEY, key, (size_t)m->keysize);
    memset(e + m->valoff, 0, (size_t)m->valsize);
    goiaba_map_link(m, i, hash);
    m->count++;
    return e + m->valoff;
}

void goiaba_map_delete(goiaba_map *m, const void *key) {
    if (m == NULL || m->count == 0) return;
    int64_t i = goiaba_map_find(m, key, goiaba_map_hash(m, key));
    if (i < 0) return;
    uint8_t *e = goiaba_map_entry(m, i);
    e[GOIABA_MAP_LIVE] = 0;
    memset(e + GOIABA_MAP_KEY, 0, (size_t)(m->entsize - GOIABA_MAP_KEY));
    m->count--;
}

// Advances a range loop. `*cursor` starts at 0. Returns 0 when done,
// otherwise stores pointers to the next entry's key and value.
int32_t goiaba_map_next(const goiaba_map *m, int64_t *cursor, void **key, void **val) {
    if (m == NULL) return 0;
    while (*cursor < m->used) {
        uint8_t *e = goiaba_map_entry(m, (*cursor)++);
        if (e[GOIABA_MAP_LIVE]) {
            *key = e + GOIABA_MAP_KEY;
            *val = e + m->valoff;
            return 1;
        }
    }
    return 0;
}

// ---- program start and exit ----------------------------------------------------

static int goiaba_argc;
static char **goiaba_argv;

// Pointers the runtime keeps in C variables, which the collector must see.
static void goiaba_scan_runtime_roots(void) {
    goiaba_scan((const uint8_t *)&goiaba_itabs, (const uint8_t *)(&goiaba_itabs + 1));
    goiaba_scan((const uint8_t *)&goiaba_panics, (const uint8_t *)(&goiaba_panics + 1));
    goiaba_scan((const uint8_t *)&goiaba_frames, (const uint8_t *)(&goiaba_frames + 1));
}

// Called first thing in main with the process arguments. Its frame address
// is the top of the stack range the collector scans.
void goiaba_init(int32_t argc, char **argv) {
    goiaba_argc = argc;
    goiaba_argv = argv;
    goiaba_stack_base = (uint8_t *)__builtin_frame_address(0) + 512;
    const char *gc = getenv("GOIABA_GC");
    if (gc != NULL && strcmp(gc, "off") == 0) goiaba_gc_enabled = 0;
    goiaba_gc_trace = getenv("GOIABA_GCTRACE") != NULL;
}

GOIABA_NORETURN void goiaba_exit(int32_t code) {
    fflush(stdout);
    exit(code);
}

// ---- support for the standard library --------------------------------------------
//
// A Go function declared without a body in package `p` is the C function
// goiaba_p_<name> below. Strings arrive as (pointer, length), slices as
// (pointer, length, capacity) and interface values as (type, data).

#include <math.h>

static void goiaba_set_string(goiaba_string *out, const char *text, int64_t len) {
    uint8_t *copy = goiaba_alloc(len);
    memcpy(copy, text, (size_t)len);
    out->ptr = copy;
    out->len = len;
}

// -- os

void goiaba_os_exit(int64_t code) { goiaba_exit((int32_t)code); }

// Writes straight to the descriptor, unbuffered like Go's os.File.
int64_t goiaba_os_write(int64_t fd, const uint8_t *p, int64_t len, int64_t cap) {
    (void)cap;
    int64_t done = 0;
    while (done < len) {
        int64_t n = (int64_t)goiaba_fd_write((int)fd, p + done, (unsigned)(len - done));
        if (n <= 0) break;
        done += n;
    }
    return done;
}

int64_t goiaba_os_nargs(void) { return goiaba_argc; }

void goiaba_os_arg(int64_t i, goiaba_string *out) {
    const char *arg = goiaba_argv[i];
    goiaba_set_string(out, arg, (int64_t)strlen(arg));
}

int32_t goiaba_os_getenv(const uint8_t *name, int64_t len, goiaba_string *out) {
    char *key = goiaba_alloc(len + 1);
    memcpy(key, name, (size_t)len);
    const char *value = getenv(key);
    if (value == NULL) return 0;
    goiaba_set_string(out, value, (int64_t)strlen(value));
    return 1;
}

// -- math

double goiaba_math_sqrt(double x) { return sqrt(x); }
double goiaba_math_floor(double x) { return floor(x); }
double goiaba_math_ceil(double x) { return ceil(x); }
double goiaba_math_trunc(double x) { return trunc(x); }
double goiaba_math_pow(double x, double y) { return pow(x, y); }
double goiaba_math_exp(double x) { return exp(x); }
double goiaba_math_log(double x) { return log(x); }
double goiaba_math_log2(double x) { return log2(x); }
double goiaba_math_log10(double x) { return log10(x); }
double goiaba_math_sin(double x) { return sin(x); }
double goiaba_math_cos(double x) { return cos(x); }
double goiaba_math_tan(double x) { return tan(x); }
double goiaba_math_atan(double x) { return atan(x); }
double goiaba_math_atan2(double y, double x) { return atan2(y, x); }
double goiaba_math_mod(double x, double y) { return fmod(x, y); }
double goiaba_math_inf(int64_t sign) { return sign >= 0 ? INFINITY : -INFINITY; }
double goiaba_math_nan(void) { return NAN; }

// -- strconv

// Decimal digits of |v|, without trailing zeros, and the position of the
// decimal point relative to them (value = 0.d1d2... * 10^point).
typedef struct {
    char digits[24];
    int nd;
    int point;
} goiaba_decimal;

// The shortest digits that parse back to exactly v (as a float32 when bits is 32).
static void goiaba_shortest(double v, int bits, goiaba_decimal *d) {
    char tmp[40];
    for (int p = 0; p < 17; p++) {
        snprintf(tmp, sizeof tmp, "%.*e", p, v);
        double back = strtod(tmp, NULL);
        if (bits == 32 ? (float)back == (float)v : back == v) break;
    }
    d->nd = 0;
    char *e = strchr(tmp, 'e');
    for (char *c = tmp; c < e; c++) {
        if (*c >= '0' && *c <= '9') d->digits[d->nd++] = *c;
    }
    d->point = atoi(e + 1) + 1;
    while (d->nd > 1 && d->digits[d->nd - 1] == '0') d->nd--;
    if (d->nd == 1 && d->digits[0] == '0') d->point = 1; // zero
}

static int goiaba_put(char *out, int n, char c) {
    out[n] = c;
    return n + 1;
}

// d.ddde±dd with `prec` digits after the point.
static int goiaba_format_e(char *out, int n, const goiaba_decimal *d, int prec, char e) {
    n = goiaba_put(out, n, d->digits[0]);
    if (prec > 0) {
        n = goiaba_put(out, n, '.');
        for (int i = 1; i <= prec; i++) n = goiaba_put(out, n, i < d->nd ? d->digits[i] : '0');
    }
    int exp = d->point - 1;
    if (d->nd == 1 && d->digits[0] == '0') exp = 0;
    n = goiaba_put(out, n, e);
    n = goiaba_put(out, n, exp < 0 ? '-' : '+');
    if (exp < 0) exp = -exp;
    n += snprintf(out + n, 8, "%02d", exp);
    return n;
}

// ddd.ddd with `prec` digits after the point.
static int goiaba_format_f(char *out, int n, const goiaba_decimal *d, int prec) {
    if (d->point > 0) {
        for (int i = 0; i < d->point; i++) n = goiaba_put(out, n, i < d->nd ? d->digits[i] : '0');
    } else {
        n = goiaba_put(out, n, '0');
    }
    if (prec > 0) {
        n = goiaba_put(out, n, '.');
        for (int i = 0; i < prec; i++) {
            int j = d->point + i;
            n = goiaba_put(out, n, j >= 0 && j < d->nd ? d->digits[j] : '0');
        }
    }
    return n;
}

// strconv.FormatFloat. Writes at most `cap` bytes to buf and returns the count.
// A negative precision means the shortest representation that round-trips.
int64_t goiaba_strconv_formatFloat(double v, int64_t fmt, int64_t prec, int64_t bits, uint8_t *buf,
                                   int64_t len, int64_t cap) {
    (void)len;
    char out[400];
    int n = 0;
    if (v != v) {
        n = snprintf(out, sizeof out, "NaN");
    } else if (isinf(v)) {
        n = snprintf(out, sizeof out, v > 0 ? "+Inf" : "-Inf");
    } else if (prec >= 0) {
        if (prec > 300) prec = 300;
        const char *spec = fmt == 'e' ? "%.*e" : fmt == 'E' ? "%.*E" : fmt == 'f' ? "%.*f" : "%.*g";
        if ((fmt == 'g' || fmt == 'G') && prec == 0) prec = 1;
        n = snprintf(out, sizeof out, spec, (int)prec, v);
    } else {
        if (signbit(v)) {
            n = goiaba_put(out, n, '-');
            v = -v;
        }
        goiaba_decimal d;
        goiaba_shortest(v, (int)bits, &d);
        int exp = d.point - 1;
        if (fmt == 'e' || fmt == 'E') {
            n = goiaba_format_e(out, n, &d, d.nd - 1, (char)fmt);
        } else if (fmt == 'f') {
            n = goiaba_format_f(out, n, &d, d.nd > d.point ? d.nd - d.point : 0);
        } else if (exp < -4 || exp >= 6) {
            // %g: with the shortest form Go uses an exponent from 1e6 up.
            n = goiaba_format_e(out, n, &d, d.nd - 1, fmt == 'G' ? 'E' : 'e');
        } else {
            n = goiaba_format_f(out, n, &d, d.nd > d.point ? d.nd - d.point : 0);
        }
    }
    if (n > cap) n = (int)cap;
    memcpy(buf, out, (size_t)n);
    return n;
}

// strconv.ParseFloat. Returns 0 when s is not a number.
int32_t goiaba_strconv_parseFloat(const uint8_t *s, int64_t len, double *out) {
    if (len == 0 || len > 400) return 0;
    char tmp[404];
    memcpy(tmp, s, (size_t)len);
    tmp[len] = 0;
    char *end;
    *out = strtod(tmp, &end);
    return end == tmp + len;
}

// -- fmt and sort: looking inside interface values

// Builds the interface value for the `type` stored at `addr`.
static void goiaba_make_any(const goiaba_type *type, void *addr, void **out) {
    if (type->kind == GOIABA_KIND_INTERFACE) {
        void **words = addr;
        const void *first = words[0];
        if (first != NULL && (type->flags & GOIABA_FLAG_METHODS)) first = *(void **)first;
        out[0] = (void *)first;
        out[1] = words[1];
    } else if (type->flags & GOIABA_FLAG_DIRECT) {
        out[0] = (void *)type;
        out[1] = *(void **)addr;
    } else {
        out[0] = (void *)type;
        out[1] = addr;
    }
}

int64_t goiaba_fmt_kind(const goiaba_type *t, void *d) {
    (void)d;
    return t == NULL ? 0 : t->kind;
}

int64_t goiaba_fmt_size(const goiaba_type *t, void *d) {
    (void)d;
    return t->size;
}

int64_t goiaba_fmt_intOf(const goiaba_type *t, void *d) {
    switch (t->size) {
    case 1: return *(int8_t *)d;
    case 2: return *(int16_t *)d;
    case 4: return *(int32_t *)d;
    default: return *(int64_t *)d;
    }
}

uint64_t goiaba_fmt_uintOf(const goiaba_type *t, void *d) {
    switch (t->size) {
    case 1: return *(uint8_t *)d;
    case 2: return *(uint16_t *)d;
    case 4: return *(uint32_t *)d;
    default: return *(uint64_t *)d;
    }
}

double goiaba_fmt_floatOf(const goiaba_type *t, void *d) {
    return t->size == 4 ? *(float *)d : *(double *)d;
}

int32_t goiaba_fmt_boolOf(const goiaba_type *t, void *d) {
    (void)t;
    return *(uint8_t *)d & 1;
}

void goiaba_fmt_stringOf(const goiaba_type *t, void *d, goiaba_string *out) {
    (void)t;
    *out = *(goiaba_string *)d;
}

void goiaba_fmt_typeName(const goiaba_type *t, void *d, goiaba_string *out) {
    (void)d;
    if (t == NULL) {
        goiaba_set_string(out, "<nil>", 5);
        return;
    }
    out->ptr = t->name;
    out->len = t->name_len;
}

// The address a pointer, map or function value holds; 0 for nil.
uint64_t goiaba_fmt_pointerOf(const goiaba_type *t, void *d) {
    if (t->flags & GOIABA_FLAG_DIRECT) return (uint64_t)(uintptr_t)d;
    return (uint64_t)(uintptr_t) * (void **)d;
}

// Number of elements of a slice, array or map, or fields of a struct.
int64_t goiaba_fmt_length(const goiaba_type *t, void *d) {
    switch (t->kind) {
    case GOIABA_KIND_SLICE: return ((int64_t *)d)[1];
    case GOIABA_KIND_MAP: return goiaba_map_len(*(goiaba_map **)d);
    default: return t->count;
    }
}

// Element i of a slice or array, field i of a struct, or (i = 0) what a pointer points to.
void goiaba_fmt_elem(const goiaba_type *t, void *d, int64_t i, void **out) {
    switch (t->kind) {
    case GOIABA_KIND_SLICE:
        goiaba_make_any(t->elem, *(uint8_t **)d + i * t->elem->size, out);
        return;
    case GOIABA_KIND_ARRAY:
        goiaba_make_any(t->elem, (uint8_t *)d + i * t->elem->size, out);
        return;
    case GOIABA_KIND_STRUCT: {
        const goiaba_field *f = &((const goiaba_field *)t->extra)[i];
        goiaba_make_any(f->type, (uint8_t *)d + f->offset, out);
        return;
    }
    case GOIABA_KIND_POINTER:
        goiaba_make_any(t->elem, d, out);
        return;
    default:
        out[0] = NULL;
        out[1] = NULL;
    }
}

void goiaba_fmt_fieldName(const goiaba_type *t, void *d, int64_t i, goiaba_string *out) {
    (void)d;
    const goiaba_field *f = &((const goiaba_field *)t->extra)[i];
    out->ptr = f->name;
    out->len = f->name_len;
}

// Kind of the element (or pointee) type; 0 when there is none.
int64_t goiaba_fmt_elemKind(const goiaba_type *t, void *d) {
    (void)d;
    return t->elem == NULL ? 0 : t->elem->kind;
}

// Steps through a map. `*cursor` starts at 0. Returns 0 when done.
int32_t goiaba_fmt_mapNext(const goiaba_type *t, void *d, int64_t *cursor, void **key, void **val) {
    void *k, *v;
    if (!goiaba_map_next(*(goiaba_map **)d, cursor, &k, &v)) return 0;
    goiaba_make_any((const goiaba_type *)t->extra, k, key);
    goiaba_make_any(t->elem, v, val);
    return 1;
}

int64_t goiaba_sort_length(const goiaba_type *t, void *d) { return goiaba_fmt_length(t, d); }

// Swaps elements i and j of the slice in the interface value.
void goiaba_sort_swap(const goiaba_type *t, void *d, int64_t i, int64_t j) {
    if (t == NULL || t->kind != GOIABA_KIND_SLICE) goiaba_panic_message("sort: argument is not a slice");
    int64_t size = t->elem->size;
    uint8_t *base = *(uint8_t **)d;
    uint8_t *a = base + i * size, *b = base + j * size;
    for (int64_t k = 0; k < size; k++) {
        uint8_t tmp = a[k];
        a[k] = b[k];
        b[k] = tmp;
    }
}

// -- runtime

void goiaba_runtime_gc(void) { goiaba_collect(); }

// Fills a runtime.MemStats: { Alloc, TotalAlloc, HeapAlloc uint64; NumGC uint32 }.
void goiaba_runtime_readMemStats(uint64_t *stats) {
    uint64_t in_use = (uint64_t)(goiaba_live_bytes + goiaba_since_gc);
    stats[0] = in_use;
    stats[1] = goiaba_total_alloc;
    stats[2] = in_use;
    memcpy(&stats[3], &goiaba_num_gc, sizeof goiaba_num_gc);
}
