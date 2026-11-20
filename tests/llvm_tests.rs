// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

use goiaba::llvm::compile_str;

fn compile(source: &str) -> String {
    compile_str(source).expect("Go source should compile to LLVM IR")
}

fn compile_err(source: &str) -> String {
    compile_str(source)
        .expect_err("Go source should be rejected")
        .to_string()
}

#[test]
fn exported_function_keeps_its_name_and_linkage() {
    let ir = compile(
        r#"
        package main

        //export add
        func add(x int, y int) int {
            return x + y
        }

        func helper(x int) int {
            return x * 2
        }
        "#,
    );
    assert!(ir.contains("define i64 @\"add\"(i64 %arg0, i64 %arg1)"));
    assert!(ir.contains("define internal i64 @\"main.helper\"(i64 %arg0)"));
    assert!(!ir.contains("define i32 @main("));
}

#[test]
fn main_function_gets_a_c_entry_point() {
    let ir = compile("package main\nfunc main() {}\n");
    assert!(ir.contains("define internal void @\"main.main\"()"));
    assert!(ir.contains("define i32 @main(i32 %argc, ptr %argv)"));
}

#[test]
fn structs_become_named_llvm_types() {
    let ir = compile(
        r#"
        package main

        type Point struct {
            x int
            y float64
            name string
        }

        func origin() Point {
            return Point{}
        }
        "#,
    );
    assert!(ir.contains("%\"main.Point\" = type { i64, double, %string }"));
}

#[test]
fn go_examples_compile() {
    for entry in std::fs::read_dir("go_examples").unwrap() {
        let path = entry.unwrap().path();
        let source = std::fs::read_to_string(&path).unwrap();
        if let Err(e) = compile_str(&source) {
            panic!("{} failed to compile: {e}", path.display());
        }
    }
}

#[test]
fn unsupported_constructs_are_errors() {
    let jump = compile_err("package main\nfunc main() {\n\tgoto done\ndone:\n}\n");
    assert!(jump.contains("goto is not supported yet"));

    let fall =
        compile_err("package main\nfunc main() { switch 1 { case 1: fallthrough\n case 2: } }\n");
    assert!(fall.contains("fallthrough is not supported yet"));

    let package = compile_err(
        "package main\nimport \"net/http\"\nfunc main() { http.ListenAndServe(\":80\", nil) }\n",
    );
    assert!(package.contains("package net/http is not available"));
}

#[test]
fn type_errors_are_reported() {
    let mismatch = compile_err(
        "package main\nfunc main() { var a int = 1\n var b float64 = 2\n println(a + b) }\n",
    );
    assert!(mismatch.contains("mismatched types int and float64"));

    let typed = compile_err("package main\nfunc main() { println(uint16(65535) + 1) }\n");
    assert!(typed.contains("constant 65536 overflows uint16"));

    let overflow = compile_err("package main\nfunc main() { var a uint8 = 300\n _ = a }\n");
    assert!(overflow.contains("constant 300 overflows uint8"));

    let undefined = compile_err("package main\nfunc main() { println(missing) }\n");
    assert!(undefined.contains("undefined: missing"));

    let located = compile_err(
        "package main\n\nfunc main() {\n\tx := 1\n\tvar y float64 = 2\n\tprintln(x + y)\n}\n",
    );
    assert!(located.starts_with("main.go:6:10: "), "{located}");

    let distinct = compile_err(
        "package main\ntype Celsius float64\nfunc main() { var c Celsius = 1\n var f float64 = c\n _ = f }\n",
    );
    assert!(distinct.contains("cannot use Celsius as float64"));

    let zero = compile_err("package main\nfunc main() { println(1 / 0) }\n");
    assert!(zero.contains("division by zero"));
}

/// Tests that build and run the generated IR. They need a C compiler
/// (`$CC` or `clang`) and are skipped when it is not installed.
mod execution {
    use goiaba::llvm::{Options, build, compile_sources};
    use std::process::Command;

    struct Run {
        stdout: String,
        stderr: String,
        code: Option<i32>,
    }

    fn run(source: &str) -> Option<Vec<Run>> {
        let cc = build::default_cc();
        let Ok((triple, layout)) = build::query_target(&cc, None) else {
            eprintln!("{cc} not usable, skipping execution test");
            return None;
        };
        let opts = Options {
            target: Some(triple),
            data_layout: Some(layout),
            module: None,
        };
        let files = [("main.go".to_string(), source.to_string())];
        let ir = compile_sources(&files, &opts).expect("Go source should compile to LLVM IR");
        let dir = tempfile::tempdir().unwrap();

        // Both levels, so that reliance on undefined behaviour shows up as a difference.
        let mut runs = Vec::new();
        for opt in ["-O0", "-O2"] {
            let exe = dir.path().join(format!("main{opt}"));
            if let Err(e) = build::build_executable(&cc, &ir, &exe, &[opt.to_string()]) {
                panic!("building at {opt} failed: {e}");
            }
            let out = Command::new(&exe).output().unwrap();
            runs.push(Run {
                stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
                stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
                code: out.status.code(),
            });
        }
        Some(runs)
    }

    /// Runs `source` and checks what `println` wrote and the exit code.
    fn expect(source: &str, stderr: &str, code: i32) {
        let Some(runs) = run(source) else { return };
        for r in runs {
            assert_eq!(r.stderr, stderr);
            assert_eq!(r.code, Some(code));
        }
    }

    /// Like `expect`, for programs that write to standard output.
    fn expect_stdout(source: &str, stdout: &str, code: i32) {
        let Some(runs) = run(source) else { return };
        for r in runs {
            assert_eq!(r.stdout, stdout, "stderr: {}", r.stderr);
            assert_eq!(r.code, Some(code));
        }
    }

    #[test]
    fn arithmetic_and_control_flow() {
        expect(
            r#"
            package main

            func fib(n int) int {
                if n <= 1 {
                    return n
                }
                return fib(n-1) + fib(n-2)
            }

            func gcd(a int, b int) int {
                for b != 0 {
                    a, b = b, a%b
                }
                return a
            }

            func grade(score int) string {
                switch {
                case score >= 90:
                    return "A"
                case score >= 80:
                    return "B"
                default:
                    return "F"
                }
            }

            func main() {
                println(fib(20), gcd(84, 36))
                println(grade(95), grade(85), grade(10))
                total := 0
                for i := 0; i < 10; i++ {
                    if i%2 == 0 {
                        continue
                    }
                    if i > 7 {
                        break
                    }
                    total += i
                }
                println(total)
                for i := range 3 {
                    switch i {
                    case 0, 2:
                        println("even", i)
                    default:
                        println("odd", i)
                    }
                }
                println(-7/2, -7%2, 7&^5, 1<<10, 0x10, 'a')
                println(true && !false, 3 > 2 || 1/total == 0)
            }
            "#,
            "6765 12\nA B F\n16\neven 0\nodd 1\neven 2\n-3 -1 2 1024 16 97\ntrue true\n",
            0,
        );
    }

    #[test]
    fn structs_methods_and_pointers() {
        expect(
            r#"
            package main

            type Point struct {
                x int
                y int
            }

            type Line struct {
                from Point
                to   Point
            }

            func (p Point) Sum() int { return p.x + p.y }

            func (p *Point) Scale(k int) {
                p.x *= k
                p.y *= k
            }

            func bump(n *int) { *n = *n + 1 }

            func newPoint(x int, y int) *Point {
                p := Point{x, y}
                return &p
            }

            func divmod(a int, b int) (int, int) {
                return a / b, a % b
            }

            func main() {
                p := Point{x: 3, y: 4}
                p.Scale(2)
                println(p.Sum(), p.x, p.y)

                q := newPoint(1, 2)
                q.y = 40
                println(q.Sum(), q == nil)

                line := Line{from: Point{1, 2}, to: Point{y: 9}}
                line.to.x = line.from.y + 5
                println(line.to.Sum())

                n := 5
                bump(&n)
                println(n)

                d, m := divmod(17, 5)
                println(d, m)

                var empty *Point
                println(empty == nil)
            }
            "#,
            "14 6 8\n41 false\n16\n6\n3 2\ntrue\n",
            0,
        );
    }

    #[test]
    fn slices_arrays_and_strings() {
        expect(
            r#"
            package main

            const Limit = 4

            var calls int

            func sum(xs []int) int {
                calls++
                total := 0
                for _, x := range xs {
                    total += x
                }
                return total
            }

            func main() {
                xs := []int{1, 2, 3}
                xs = append(xs, 4, 5)
                println(sum(xs), len(xs), cap(xs))

                var squares [Limit]int
                for i := 0; i < len(squares); i++ {
                    squares[i] = i * i
                }
                println(squares[3], sum(squares[1:3]), calls)

                grid := make([]int, 2, 8)
                grid[1] = 9
                println(grid[1], len(grid), cap(grid), grid == nil)

                var none []int
                println(none == nil, len(none))

                s := "hello" + ", " + "world"
                s += "!"
                println(s, len(s), s[0], s[7:12])
                println(s == "hello, world!", s != "x", "abc" < "abd", "ab" < "abc")
                println("tab\there", `raw\n`)
            }
            "#,
            "15 5 6\n9 5 2\n9 2 8 false\ntrue 0\nhello, world! 13 104 world\ntrue true true true\ntab\there raw\\n\n",
            0,
        );
    }

    #[test]
    fn integer_edge_cases_follow_go() {
        expect(
            r#"
            package main

            func main() {
                min := -9223372036854775807 - 1
                minusOne := -1
                println(min/minusOne, min%minusOne)

                var count uint = 70
                one := 1
                neg := -8
                println(one<<count, neg>>count, neg>>1)

                var small uint8 = 250
                small += 10
                var top uint16 = 65535
                println(small, int8(small+200), top+1)

                f := 2.5
                println(int(f*2), f > 2, float32(1)/4 == 0.25)
            }
            "#,
            "-9223372036854775808 0\n0 -1 -4\n4 -52 0\n5 true true\n",
            0,
        );
    }

    #[test]
    fn defined_types_constants_and_results() {
        expect(
            r#"
            package main

            type Celsius float64
            type Weekday int
            type IntList []int

            const (
                Sunday Weekday = iota
                Monday
                Tuesday
            )

            const (
                KB = 1 << (10 * (iota + 1))
                MB
            )

            func (c Celsius) Fahrenheit() float64 { return float64(c)*9/5 + 32 }

            func (d Weekday) Next() Weekday { return (d + 1) % 7 }

            func (l IntList) Sum() int {
                total := 0
                for _, v := range l {
                    total += v
                }
                return total
            }

            func split(sum int) (x, y int) {
                x = sum * 4 / 9
                y = sum - x
                return
            }

            func total(prefix string, nums ...int) (n int) {
                for _, v := range nums {
                    n += v
                }
                println(prefix, len(nums))
                return n
            }

            func main() {
                var c Celsius = 100
                println(c.Fahrenheit(), Tuesday, Monday.Next(), KB, MB)
                l := IntList{1, 2, 3}
                l = append(l, 4)
                println(l.Sum())
                a, b := split(17)
                println(a, b)
                println(total("none"), total("three", 1, 2, 3), total("spread", l...))
                println(1.5, -0.25, 1e100, 0.0)
            }
            "#,
            "+2.120000e+002 2 2 1024 1048576\n10\n7 10\nnone 0\nthree 3\nspread 4\n0 6 10\n\
             +1.500000e+000 -2.500000e-001 +1.000000e+100 +0.000000e+000\n",
            0,
        );
    }

    #[test]
    fn runes_and_byte_conversions() {
        expect(
            r#"
            package main

            func main() {
                for i, r := range "h\u00e9llo, \u4e16\u754c" {
                    print(i, ":", r, " ")
                }
                println()
                s := "go"
                bs := []byte(s)
                bs[0] = 'G'
                bs = append(bs, "lang"...)
                println(string(bs), s, string(rune(19990)) == "\u4e16", string(bs[1:3]))
            }
            "#,
            "0:104 1:233 3:108 4:108 5:111 6:44 7:32 8:19990 11:30028 \nGolang go true ol\n",
            0,
        );
    }

    #[test]
    fn nil_pointer_dereference_panics() {
        expect(
            r#"
            package main

            type Node struct {
                val  int
                next *Node
            }

            func main() {
                var n *Node
                println(n.val)
            }
            "#,
            "panic: runtime error: invalid memory address or nil pointer dereference\n\n",
            2,
        );
    }

    #[test]
    fn closures_and_function_values() {
        expect(
            r#"
            package main

            type Op func(int, int) int

            type Button struct {
            	label   string
            	onClick func(string)
            }

            func add(a, b int) int { return a + b }

            func apply(f Op, a, b int) int { return f(a, b) }

            func counter() func() int {
            	n := 0
            	return func() int {
            		n++
            		return n
            	}
            }

            func compose(f, g func(int) int) func(int) int {
            	return func(x int) int { return g(f(x)) }
            }

            func mapInts(xs []int, f func(int) int) []int {
            	out := make([]int, 0, len(xs))
            	for _, x := range xs {
            		out = append(out, f(x))
            	}
            	return out
            }

            func main() {
            	println(apply(add, 2, 3), apply(func(a, b int) int { return a * b }, 4, 5))
            	next := counter()
            	other := counter()
            	println(next(), next(), next(), other())

            	double := func(x int) int { return x * 2 }
            	inc := func(x int) int { return x + 1 }
            	println(compose(double, inc)(5), compose(inc, double)(5))

            	factor := 3
            	scaled := mapInts([]int{1, 2, 3}, func(x int) int { return x * factor })
            	println(scaled[0], scaled[1], scaled[2])

            	var fns []func() int
            	for i := 0; i < 3; i++ {
            		fns = append(fns, func() int { return i * 10 })
            	}
            	for _, v := range []int{7, 8} {
            		fns = append(fns, func() int { return v })
            	}
            	for _, f := range fns {
            		print(f(), " ")
            	}
            	println()

            	b := Button{label: "ok", onClick: func(s string) { println("clicked", s) }}
            	b.onClick(b.label)

            	var nilf func()
            	println(nilf == nil, next != nil)

            	total := 0
            	func() {
            		for i := 1; i <= 4; i++ {
            			func() { total += i }()
            		}
            	}()
            	println(total)
            }
            "#,
            "5 20\n1 2 3 1\n11 12\n3 6 9\n0 10 20 7 8 \nclicked ok\ntrue true\n10\n",
            0,
        );
    }

    #[test]
    fn index_out_of_range_panics() {
        expect(
            r#"
            package main

            func main() {
                xs := []int{1, 2, 3}
                i := 5
                println(xs[i])
            }
            "#,
            "panic: runtime error: index out of range [5] with length 3\n\n",
            2,
        );
    }

    #[test]
    fn divide_by_zero_panics() {
        expect(
            r#"
            package main

            func main() {
                zero := 0
                println(10 / zero)
            }
            "#,
            "panic: runtime error: integer divide by zero\n\n",
            2,
        );
    }

    #[test]
    fn explicit_panic_exits_with_two() {
        expect(
            r#"
            package main

            func main() {
                println("before")
                panic("boom")
            }
            "#,
            "before\npanic: boom\n\n",
            2,
        );
    }

    #[test]
    fn interfaces_and_type_switches() {
        expect(
            r#"
            package main

            type Shape interface {
            	Area() float64
            	Name() string
            }

            type Named interface {
            	Name() string
            }

            type Rect struct{ w, h float64 }
            type Circle struct{ r float64 }

            func (r Rect) Area() float64    { return r.w * r.h }
            func (r Rect) Name() string     { return "rect" }
            func (c *Circle) Area() float64 { return 3 * c.r * c.r }
            func (c *Circle) Name() string  { return "circle" }

            type MyErr struct{ code int }

            func (e MyErr) Error() string { return "my error" }

            func find(code int) error {
            	if code == 0 {
            		return nil
            	}
            	return MyErr{code}
            }

            func describe(v any) string {
            	switch x := v.(type) {
            	case nil:
            		return "nil"
            	case int:
            		if x > 10 {
            			return "big int"
            		}
            		return "int"
            	case string:
            		return "string " + x
            	case bool, float64:
            		return "bool or float"
            	case Named:
            		return "named " + x.Name()
            	case error:
            		return "error " + x.Error()
            	default:
            		return "other"
            	}
            }

            func total(shapes []Shape) float64 {
            	sum := 0.0
            	for _, s := range shapes {
            		sum += s.Area()
            	}
            	return sum
            }

            func main() {
            	shapes := []Shape{Rect{2, 3}, &Circle{1}}
            	println(total(shapes), shapes[0].Name(), shapes[1].Name())

            	var n Named = shapes[1]
            	println(n.Name())

            	err := find(0)
            	println(err == nil)
            	err = find(7)
            	println(err != nil, err.Error())
            	if me, ok := err.(MyErr); ok {
            		println("code", me.code)
            	}
            	_, isShape := err.(Shape)
            	println(isShape)

            	println(describe(nil), describe(5), describe(50), describe("x"))
            	println(describe(true), describe(1.5), describe(Rect{1, 1}), describe(err), describe([]int{1}))

            	var a, b any = 3, 3
            	println(a == b, a == any(4), a == any("3"), Rect{1, 2} == Rect{1, 2}, Rect{1, 2} != Rect{2, 1})
            	var s Shape = Rect{1, 2}
            	println(s == Shape(Rect{1, 2}), s.(Rect).w)

            	c := a.(int) + 1
            	println(c)
            	println(a.(string))
            }
            "#,
            "+9.000000e+000 rect circle\ncircle\ntrue\ntrue my error\ncode 7\nfalse\nnil int big int string x\nbool or float bool or float named rect error my error other\ntrue false false true true\ntrue +1.000000e+000\n4\npanic: interface conversion: interface {} is int, not string\n\n",
            2,
        );
    }

    #[test]
    fn defer_panic_and_recover() {
        expect(
            r#"
            package main

            type Stack struct{ items []int }

            func (s *Stack) Push(v int) { s.items = append(s.items, v) }

            type NotFound struct{ key string }

            func (e NotFound) Error() string { return "not found: " + e.key }

            func trace(name string) string {
            	println("enter", name)
            	return name
            }

            func leave(name string) { println("leave", name) }

            func order() {
            	defer leave(trace("order"))
            	for i := 0; i < 3; i++ {
            		defer println2(i)
            	}
            	println("body")
            }

            func println2(i int) { println("deferred", i) }

            func safeDiv(a, b int) (result int, err error) {
            	defer func() {
            		if r := recover(); r != nil {
            			result = -1
            			if e, ok := r.(error); ok {
            				err = e
            			}
            		}
            	}()
            	return a / b, nil
            }

            func double() (n int) {
            	defer func() { n *= 2 }()
            	return 21
            }

            func mustFind(key string) int {
            	if key == "" {
            		panic(NotFound{"<empty>"})
            	}
            	return len(key)
            }

            func lookup(key string) (n int, msg string) {
            	defer func() {
            		switch r := recover().(type) {
            		case nil:
            		case NotFound:
            			msg = "recovered " + r.Error()
            		case string:
            			msg = "string panic " + r
            		}
            	}()
            	n = mustFind(key)
            	return n, "ok"
            }


            func main() {
            	order()
            	q, err := safeDiv(10, 2)
            	println(q, err == nil)
            	q, err = safeDiv(1, 0)
            	println(q, err.Error())
            	println(double())
            	n, msg := lookup("abc")
            	println(n, msg)
            	n, msg = lookup("")
            	println(n, msg)

            	s := &Stack{}
            	func() {
            		defer s.Push(1)
            		s.Push(2)
            	}()
            	println(s.items[0], s.items[1])

            	defer println3("main exiting")
            	var xs []int
            	defer func() {
            		println("deferred before crash")
            	}()
            	println(xs[3])
            }

            func println3(s string) { println(s) }
            "#,
            "enter order\nbody\ndeferred 2\ndeferred 1\ndeferred 0\nleave order\n5 true\n-1 runtime error: integer divide by zero\n42\n3 ok\n0 recovered not found: <empty>\n2 1\ndeferred before crash\nmain exiting\npanic: runtime error: index out of range [3] with length 0\n\n",
            2,
        );
    }

    #[test]
    fn maps() {
        expect(
            r#"
            package main

            type Inventory map[string]int

            func wordCount(words []string) map[string]int {
            	counts := make(map[string]int)
            	for _, w := range words {
            		counts[w]++
            	}
            	return counts
            }

            func main() {
            	counts := wordCount([]string{"a", "b", "a", "c", "a", "b"})
            	println(len(counts), counts["a"], counts["b"], counts["zzz"])

            	if n, ok := counts["c"]; ok {
            		println("c", n)
            	}
            	_, ok := counts["missing"]
            	println(ok)

            	delete(counts, "b")
            	delete(counts, "nope")
            	for k, v := range counts {
            		println(k, v)
            	}

            	squares := map[int]int{1: 1, 2: 4, 3: 9}
            	squares[4] = 16
            	squares[2] += 100
            	sum := 0
            	for _, v := range squares {
            		sum += v
            	}
            	println(sum, len(squares))

            	inv := Inventory{"apple": 3}
            	inv["pear"] = 2
            	total := 0
            	for name := range inv {
            		total += inv[name] * len(name)
            	}
            	println(total)

            	nested := map[string][]int{}
            	nested["x"] = append(nested["x"], 1, 2)
            	println(len(nested["x"]), len(nested["y"]))

            	big := make(map[int]string, 100)
            	for i := 0; i < 1000; i++ {
            		big[i] = "v"
            	}
            	for i := 0; i < 1000; i += 2 {
            		delete(big, i)
            	}
            	println(len(big), big[999], big[998] == "")

            	var none map[string]int
            	println(none == nil, len(none), none["k"])
            	none["k"] = 1
            }
            "#,
            "3 3 2 0\nc 1\nfalse\na 3\nc 1\n130 4\n23\n2 0\n500 v true\ntrue 0 0\npanic: assignment to entry in nil map\n\n",
            2,
        );
    }

    #[test]
    fn packages_and_init() {
        expect(
            r#"
            package main

            import (
            	"errors"
            	"strings"
            )

            var ErrEmpty = errors.New("empty input")
            var separator = strings.Join([]string{"-", "-"}, "")
            var limit = 10

            type wrapped struct {
            	msg string
            	err error
            }

            func (w *wrapped) Error() string { return w.msg + ": " + w.err.Error() }
            func (w *wrapped) Unwrap() error { return w.err }

            func init() {
            	limit = limit * 2
            }

            func parse(s string) (string, error) {
            	if s == "" {
            		return "", &wrapped{"parse", ErrEmpty}
            	}
            	if !strings.HasPrefix(s, "go") {
            		return "", errors.New("bad prefix in " + s)
            	}
            	return strings.Join([]string{s, "ok"}, separator), nil
            }

            func main() {
            	out, err := parse("gopher")
            	println(out, err == nil, limit)
            	_, err = parse("")
            	println(err.Error(), errors.Is(err, ErrEmpty), err == ErrEmpty)
            	_, err = parse("rust")
            	println(err.Error(), errors.Is(err, ErrEmpty))
            	println(strings.Contains("goiaba", "iab"), strings.Index("goiaba", "z"), strings.HasSuffix("goiaba", "ba"))
            	join := strings.Join
            	println(join([]string{"a", "b"}, "+"))
            }
            "#,
            "gopher--ok true 20\nparse: empty input true false\nbad prefix in rust false\ntrue -1 true\na+b\n",
            0,
        );
    }

    #[test]
    fn standard_library() {
        expect_stdout(
            r#"
            package main

            import (
            	"errors"
            	"fmt"
            	"math"
            	"os"
            	"sort"
            	"strconv"
            	"strings"
            )

            type Point struct {
            	X, Y int
            }

            type Temp float64

            func (t Temp) String() string { return fmt.Sprintf("%.1f°C", float64(t)) }

            type byLen []string

            func (s byLen) Len() int           { return len(s) }
            func (s byLen) Less(i, j int) bool { return len(s[i]) < len(s[j]) }
            func (s byLen) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }

            var ErrNotFound = errors.New("not found")

            func find(key string) error {
            	return fmt.Errorf("find %q: %w", key, ErrNotFound)
            }

            func main() {
            	fmt.Println("hello", 42, true, 3.5, 'x', nil)
            	fmt.Println(1e6, 123456.0, 0.000001234, float32(0.1), 100.0, -2.5e-10)
            	fmt.Printf("%d|%5d|%-5d|%05d|%x|%X|%o|%b|%c|%q\n", 42, 42, 42, 42, 255, 255, 8, 5, 'A', 'B')
            	fmt.Printf("%s|%10s|%-10s|%q|%v|%.2s\n", "go", "go", "go", "go", "go", "golang")
            	fmt.Printf("%f|%.2f|%8.3f|%e|%g|%t|%v|%%\n", 3.14159, 3.14159, 3.14159, 1234.5678, 0.00001234, true, false)
            	p := Point{1, 2}
            	fmt.Println(p, &p, []int{1, 2, 3}, []string{"a", "b"}, [2]bool{true, false})
            	fmt.Printf("%v %+v %T %T %T\n", p, p, p, 1.5, "s")
            	fmt.Println(map[string]int{"a": 1, "b": 2}, []Point{{1, 2}, {3, 4}}, Temp(21.55))
            	var np *Point
            	var err error
            	fmt.Println(np, err, []int(nil), map[int]int(nil))

            	err = find("k")
            	fmt.Println(err, errors.Is(err, ErrNotFound), errors.Unwrap(err) == ErrNotFound)
            	fmt.Printf("%d %s\n", "str", 5)
            	fmt.Printf("%d\n")

            	n, err := strconv.Atoi("123")
            	fmt.Println(n+1, err)
            	_, err = strconv.Atoi("12a")
            	fmt.Println(err)
            	f, _ := strconv.ParseFloat("2.5", 64)
            	fmt.Println(f*2, strconv.Itoa(-45), strconv.FormatInt(255, 2), strconv.Quote("a\"b\n"))
            	b, _ := strconv.ParseBool("true")
            	fmt.Println(b, strconv.FormatFloat(1.0/3, 'f', 4, 64))

            	fmt.Println(strings.ToUpper("héllo"), strings.Split("a,b,c", ","), strings.Fields("  x  y z "))
            	fmt.Println(strings.TrimSpace("  pad  ")+"|", strings.Repeat("ab", 3), strings.ReplaceAll("aXbXc", "X", "-"))
            	fmt.Println(strings.Count("cheese", "e"), strings.LastIndex("go gopher", "go"), strings.Trim("xxhixx", "x"))
            	var sb strings.Builder
            	for i := 0; i < 3; i++ {
            		sb.WriteString(strconv.Itoa(i))
            		sb.WriteByte('-')
            	}
            	fmt.Println(sb.String(), sb.Len())

            	xs := []int{5, 2, 8, 1, 9, 3}
            	sort.Ints(xs)
            	words := []string{"banana", "kiwi", "apple", "fig"}
            	sort.Strings(words)
            	fmt.Println(xs, words, sort.SearchInts(xs, 8))
            	sort.Sort(byLen(words))
            	fmt.Println(words)
            	people := []Point{{3, 1}, {1, 2}, {2, 3}}
            	sort.Slice(people, func(i, j int) bool { return people[i].X < people[j].X })
            	fmt.Println(people)

            	fmt.Println(math.Sqrt(2), math.Floor(-1.5), math.Max(3, 7), math.Pow(2, 10), math.MaxInt64, math.Inf(1))
            	fmt.Printf("%.4f %v %v\n", math.Pi, math.IsNaN(math.NaN()), math.Round(2.5))
            	defer fmt.Println("deferred print")
            	os.Exit(3)
            }
            "#,
            "hello 42 true 3.5 120 <nil>\n1e+06 123456 1.234e-06 0.1 100 -2.5e-10\n42|   42|42   |00042|ff|FF|10|101|A|'B'\ngo|        go|go        |\"go\"|go|go\n3.141590|3.14|   3.142|1.234568e+03|1.234e-05|true|false|%\n{1 2} &{1 2} [1 2 3] [a b] [true false]\n{1 2} {X:1 Y:2} main.Point float64 string\nmap[a:1 b:2] [{1 2} {3 4}] 21.6°C\n<nil> <nil> [] map[]\nfind \"k\": not found true true\n%!d(string=str) %!s(int=5)\n%!d(MISSING)\n124 <nil>\nstrconv.Atoi: parsing \"12a\": invalid syntax\n5 -45 11111111 \"a\\\"b\\n\"\ntrue 0.3333\nHÉLLO [a b c] [x y z]\npad| ababab a-b-c\n3 3 hi\n0-1-2- 6\n[1 2 3 5 8 9] [apple banana fig kiwi] 4\n[fig kiwi apple banana]\n[{1 2} {2 3} {3 1}]\n1.4142135623730951 -2 7 1024 9223372036854775807 +Inf\n3.1416 true 3\n",
            3,
        );
    }

    #[test]
    fn embedding_and_labels() {
        expect_stdout(
            r#"
            package main

            import (
            	"fmt"
            	"strings"
            )

            type Animal interface {
            	Name() string
            	Sound() string
            }

            type Base struct {
            	name string
            	legs int
            }

            func (b Base) Name() string  { return b.name }
            func (b *Base) Rename(n string) { b.name = n }
            func (b Base) Describe() string {
            	return fmt.Sprintf("%s has %d legs", b.name, b.legs)
            }

            type Dog struct {
            	Base
            	breed string
            }

            func (d Dog) Sound() string { return "woof" }

            type Puppy struct {
            	*Dog
            	age int
            }

            type Logger struct {
            	strings.Builder
            	prefix string
            }

            func (l *Logger) Log(msg string) {
            	l.WriteString(l.prefix)
            	l.WriteString(msg)
            	l.WriteByte('\n')
            }

            func speak(a Animal) string { return a.Name() + " says " + a.Sound() }

            func main() {
            	d := Dog{Base{"rex", 4}, "lab"}
            	fmt.Println(d.name, d.legs, d.Base.name, d.Describe())
            	d.Rename("max")
            	d.legs++
            	fmt.Println(speak(d), d.Name(), d)

            	p := Puppy{&d, 1}
            	p.Rename("tiny")
            	fmt.Println(p.name, d.name, speak(p), speak(&p))

            	var l Logger
            	l.prefix = "> "
            	l.Log("one")
            	l.Log("two")
            	fmt.Print(l.String())
            	describe := d.Describe
            	fmt.Println(describe(), l.Len())

            	found := -1
            outer:
            	for i := 0; i < 5; i++ {
            		for j := 0; j < 5; j++ {
            			if j == 3 {
            				continue outer
            			}
            			if i*j == 4 {
            				found = i*10 + j
            				break outer
            			}
            		}
            	}
            	fmt.Println(found)
            }
            "#,
            "rex 4 rex rex has 4 legs\nmax says woof max {{max 5} lab}\ntiny tiny tiny says woof tiny says woof\n> one\n> two\ntiny has 5 legs 12\n22\n",
            0,
        );
    }

    #[test]
    fn garbage_collector_reclaims_memory() {
        expect_stdout(
            r#"
            package main

            import (
            	"fmt"
            	"runtime"
            )

            type Node struct {
            	val  int
            	next *Node
            	data []byte
            }

            var registry = map[string]*Node{}

            func build(n int) *Node {
            	var head *Node
            	for i := 0; i < n; i++ {
            		head = &Node{val: i, next: head, data: make([]byte, 64)}
            	}
            	return head
            }

            func sum(n *Node) int {
            	total := 0
            	for ; n != nil; n = n.next {
            		total += n.val + len(n.data)
            	}
            	return total
            }

            func churn(rounds int) int {
            	total := 0
            	for i := 0; i < rounds; i++ {
            		// Garbage: nothing keeps these lists alive after the iteration.
            		total += sum(build(1000))
            		s := fmt.Sprintf("round-%d", i)
            		total += len(s)
            	}
            	return total
            }

            func main() {
            	keep := build(5000)
            	registry["kept"] = build(100)
            	closures := []func() int{}
            	for i := 0; i < 50; i++ {
            		buf := make([]int, 100)
            		buf[0] = i
            		closures = append(closures, func() int { return buf[0] })
            	}

            	total := churn(3000)

            	var m runtime.MemStats
            	runtime.GC()
            	runtime.ReadMemStats(&m)
            	fmt.Println(total, sum(keep), sum(registry["kept"]))
            	acc := 0
            	for _, f := range closures {
            		acc += f()
            	}
            	fmt.Println(acc)
            	fmt.Println(m.NumGC > 5, m.HeapAlloc < 8<<20, m.TotalAlloc > 200<<20)
            }
            "#,
            "1690528890 12817500 11350\n1225\ntrue true true\n",
            0,
        );
    }

    #[test]
    fn goroutines_channels_and_select() {
        expect_stdout(
            r#"
            package main

            import (
            	"fmt"
            	"runtime"
            	"sync"
            	"sync/atomic"
            	"time"
            )

            func producer(n int, out chan<- int) {
            	for i := 0; i < n; i++ {
            		out <- i
            	}
            	close(out)
            }

            func worker(id int, jobs <-chan int, results chan<- string, wg *sync.WaitGroup) {
            	defer wg.Done()
            	for j := range jobs {
            		results <- fmt.Sprintf("w%d:%d", id, j*j)
            	}
            }

            type Counter struct {
            	mu sync.Mutex
            	n  int
            }

            func (c *Counter) Inc() {
            	c.mu.Lock()
            	defer c.mu.Unlock()
            	v := c.n
            	runtime.Gosched() // without the lock another goroutine would see a stale v
            	c.n = v + 1
            }

            func main() {
            	// Unbuffered hand-off and range over a channel.
            	ch := make(chan int)
            	go producer(5, ch)
            	sum := 0
            	for v := range ch {
            		sum += v
            	}
            	fmt.Println("sum", sum)

            	// Worker pool with a WaitGroup.
            	jobs := make(chan int, 10)
            	results := make(chan string, 10)
            	var wg sync.WaitGroup
            	for id := 1; id <= 3; id++ {
            		wg.Add(1)
            		go worker(id, jobs, results, &wg)
            	}
            	for j := 1; j <= 6; j++ {
            		jobs <- j
            	}
            	close(jobs)
            	wg.Wait()
            	close(results)
            	count := 0
            	for range results {
            		count++
            	}
            	fmt.Println("results", count, len(results), cap(results))

            	// Mutex under contention.
            	var c Counter
            	var wg2 sync.WaitGroup
            	for i := 0; i < 50; i++ {
            		wg2.Add(1)
            		go func() {
            			defer wg2.Done()
            			c.Inc()
            		}()
            	}
            	wg2.Wait()
            	fmt.Println("counter", c.n)

            	// Select: default, send, receive, closed channel, timeout.
            	a := make(chan string, 1)
            	b := make(chan string, 1)
            	select {
            	case v := <-a:
            		fmt.Println("unexpected", v)
            	default:
            		fmt.Println("nothing ready")
            	}
            	select {
            	case a <- "to a":
            		fmt.Println("sent")
            	case v := <-b:
            		fmt.Println("unexpected", v)
            	}
            	go func() {
            		time.Sleep(2 * time.Millisecond)
            		b <- "from b"
            	}()
            	for i := 0; i < 2; i++ {
            		select {
            		case v := <-a:
            			fmt.Println("got", v)
            		case v, ok := <-b:
            			fmt.Println("got", v, ok)
            		}
            	}
            	done := make(chan struct{})
            	close(done)
            	_, open := <-done
            	fmt.Println("open", open)
            	select {
            	case <-make(chan int):
            		fmt.Println("unexpected")
            	case <-time.After(3 * time.Millisecond):
            		fmt.Println("timeout")
            	}

            	// Once, atomic, and a goroutine panic recovered locally.
            	var once sync.Once
            	var hits int32
            	for i := 0; i < 3; i++ {
            		once.Do(func() { atomic.AddInt32(&hits, 1) })
            	}
            	errs := make(chan any)
            	go func() {
            		defer func() { errs <- recover() }()
            		var m map[string]int
            		m["x"] = 1
            	}()
            	fmt.Println(atomic.LoadInt32(&hits), <-errs)

            	start := time.Now()
            	time.Sleep(5 * time.Millisecond)
            	fmt.Println(time.Since(start) >= 5*time.Millisecond, runtime.NumGoroutine() >= 1)

            	var never chan int
            	<-never
            }
            "#,
            "sum 10\nresults 6 0 10\ncounter 50\nnothing ready\nsent\ngot to a\ngot from b true\nopen false\ntimeout\n1 assignment to entry in nil map\ntrue true\n",
            2,
        );
    }

    #[test]
    fn goroutines_survive_collection() {
        expect_stdout(
            r#"
            package main

            import (
            	"fmt"
            	"runtime"
            	"sync"
            )

            type item struct {
            	id   int
            	data []int
            	next *item
            }

            func work(id int, out chan<- int, wg *sync.WaitGroup) {
            	defer wg.Done()
            	var head *item
            	total := 0
            	for i := 0; i < 200; i++ {
            		head = &item{id: i, data: make([]int, 50), next: head}
            		head.data[0] = id
            		if i%20 == 0 {
            			runtime.Gosched() // park with live pointers only on this stack
            		}
            	}
            	for it := head; it != nil; it = it.next {
            		total += it.data[0] + it.id
            	}
            	out <- total
            }

            func main() {
            	const n = 300
            	out := make(chan int, n)
            	var wg sync.WaitGroup
            	for g := 0; g < n; g++ {
            		wg.Add(1)
            		go work(g, out, &wg)
            	}
            	wg.Wait()
            	close(out)
            	sum := 0
            	for v := range out {
            		sum += v
            	}
            	var m runtime.MemStats
            	runtime.ReadMemStats(&m)
            	// Each worker returns 200*id + (0+...+199).
            	fmt.Println(sum, sum == 200*(n*(n-1)/2)+n*19900, m.NumGC > 0, runtime.NumGoroutine())
            }
            "#,
            "14940000 true true 1\n",
            0,
        );
    }

    /// Builds a module laid out on disk: several packages, several files per
    /// package, imports resolved through go.mod.
    #[test]
    fn multi_package_module() {
        let files = [
            (
                "go.mod",
                r##"module example.com/todo

go 1.21
"##,
            ),
            (
                "internal/store/store.go",
                r##"// Package store keeps tasks in memory.
package store

import (
	"errors"
	"fmt"
	"sort"
	"strings"
)

// ErrNotFound is returned when a task does not exist.
var ErrNotFound = errors.New("task not found")

type Priority int

const (
	Low Priority = iota
	Medium
	High
)

func (p Priority) String() string {
	switch p {
	case Low:
		return "low"
	case Medium:
		return "medium"
	case High:
		return "high"
	}
	return fmt.Sprintf("Priority(%d)", int(p))
}

type Task struct {
	ID       int
	Title    string
	Done     bool
	Priority Priority
	Tags     []string
}

type Store struct {
	tasks  map[int]*Task
	nextID int
}

func New() *Store {
	return &Store{tasks: make(map[int]*Task), nextID: 1}
}

func (s *Store) Add(title string, p Priority, tags ...string) (*Task, error) {
	title = strings.TrimSpace(title)
	if title == "" {
		return nil, errors.New("empty title")
	}
	t := &Task{ID: s.nextID, Title: title, Priority: p, Tags: tags}
	s.tasks[t.ID] = t
	s.nextID++
	return t, nil
}

func (s *Store) Complete(id int) error {
	t, ok := s.tasks[id]
	if !ok {
		return fmt.Errorf("complete %d: %w", id, ErrNotFound)
	}
	t.Done = true
	return nil
}

func (s *Store) Len() int { return len(s.tasks) }

// Filter returns the tasks that keep accepts, highest priority first.
func (s *Store) Filter(keep func(*Task) bool) []*Task {
	var out []*Task
	for _, t := range s.tasks {
		if keep(t) {
			out = append(out, t)
		}
	}
	sort.Slice(out, func(i, j int) bool {
		if out[i].Priority != out[j].Priority {
			return out[i].Priority > out[j].Priority
		}
		return out[i].ID < out[j].ID
	})
	return out
}
"##,
            ),
            (
                "internal/render/render.go",
                r##"// Package render formats tasks for the terminal.
package render

import (
	"fmt"
	"io"
	"strings"

	"example.com/todo/internal/store"
)

type Renderer interface {
	Render(w io.Writer, tasks []*store.Task)
}

type Plain struct{}

func (Plain) Render(w io.Writer, tasks []*store.Task) {
	for _, t := range tasks {
		mark := " "
		if t.Done {
			mark = "x"
		}
		fmt.Fprintf(w, "[%s] %2d %-14s %-6v %s\n", mark, t.ID, t.Title, t.Priority, strings.Join(t.Tags, ","))
	}
}

type Summary struct {
	Width int
}

func (s Summary) Render(w io.Writer, tasks []*store.Task) {
	done := 0
	for _, t := range tasks {
		if t.Done {
			done++
		}
	}
	bar := strings.Repeat("#", done*s.Width/max(len(tasks), 1))
	fmt.Fprintf(w, "%d/%d done |%-*s|\n", done, len(tasks), s.Width, bar)
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
"##,
            ),
            (
                "cmd/todo/main.go",
                r##"package main

import (
	"fmt"
	"os"

	"example.com/todo/internal/store"
)

func must(t *store.Task, err error) *store.Task {
	if err != nil {
		fmt.Fprintln(os.Stderr, "error:", err)
		os.Exit(1)
	}
	return t
}
"##,
            ),
            (
                "cmd/todo/run.go",
                r##"package main

import (
	"errors"
	"fmt"
	"os"
	"strings"

	"example.com/todo/internal/render"
	"example.com/todo/internal/store"
)

func main() {
	s := store.New()
	must(s.Add("write compiler", store.High, "go", "llvm"))
	must(s.Add("  add a GC  ", store.Medium, "runtime"))
	must(s.Add("ship it", store.Low))
	if _, err := s.Add("   ", store.Low); err != nil {
		fmt.Println("rejected:", err)
	}

	if err := s.Complete(1); err != nil {
		fmt.Println(err)
	}
	err := s.Complete(42)
	fmt.Println(err, errors.Is(err, store.ErrNotFound))

	renderers := []render.Renderer{render.Plain{}, render.Summary{Width: 9}}
	all := s.Filter(func(*store.Task) bool { return true })
	for _, r := range renderers {
		r.Render(os.Stdout, all)
	}

	tagged := s.Filter(func(t *store.Task) bool {
		for _, tag := range t.Tags {
			if strings.HasPrefix(tag, "ll") {
				return true
			}
		}
		return false
	})
	fmt.Println(len(tagged), tagged[0].Title, s.Len(), store.High, store.Priority(7))
}
"##,
            ),
        ];
        let dir = tempfile::tempdir().unwrap();
        for (path, text) in files {
            let path = dir.path().join(path);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, text).unwrap();
        }
        let main_dir = dir.path().join("cmd/todo");
        let cc = build::default_cc();
        let Ok((triple, layout)) = build::query_target(&cc, None) else {
            eprintln!("{cc} not usable, skipping execution test");
            return;
        };
        let opts = Options {
            target: Some(triple),
            data_layout: Some(layout),
            module: goiaba::llvm::Module::find(&main_dir),
        };
        assert_eq!(opts.module.as_ref().unwrap().path, "example.com/todo");
        let sources = goiaba::llvm::read_package_dir(&main_dir).unwrap();
        let ir = compile_sources(&sources, &opts).expect("module should compile");
        let exe = dir.path().join("todo");
        build::build_executable(&cc, &ir, &exe, &["-O2".to_string()]).unwrap();
        let out = Command::new(&exe).output().unwrap();
        assert_eq!(
            String::from_utf8_lossy(&out.stdout),
            "rejected: empty title\ncomplete 42: task not found true\n[x]  1 write compiler high   go,llvm\n[ ]  2 add a GC       medium runtime\n[ ]  3 ship it        low    \n1/3 done |###      |\n1 write compiler 3 high Priority(7)\n"
        );
        assert_eq!(out.status.code(), Some(0));
    }
}
