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
    assert!(!ir.contains("define i32 @main()"));
}

#[test]
fn main_function_gets_a_c_entry_point() {
    let ir = compile("package main\nfunc main() {}\n");
    assert!(ir.contains("define internal void @\"main.main\"()"));
    assert!(ir.contains("define i32 @main()"));
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
    let goroutine = compile_err("package main\nfunc f() {}\nfunc main() { go f() }\n");
    assert!(goroutine.contains("goroutines are not supported yet"));

    let map = compile_err("package main\nfunc main() { m := make(map[string]int)\n _ = m }\n");
    assert!(map.contains("map types are not supported yet"));

    let package = compile_err(
        "package main\nimport \"strings\"\nfunc main() { println(strings.ToUpper(\"a\")) }\n",
    );
    assert!(package.contains("package strings is not available"));
}

#[test]
fn type_errors_are_reported() {
    let mismatch = compile_err(
        "package main\nfunc main() { var a int = 1\n var b float64 = 2\n println(a + b) }\n",
    );
    assert!(mismatch.contains("mismatched types int and float64"));

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
                println(small, int8(small+200), uint16(65535)+1)

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
}
