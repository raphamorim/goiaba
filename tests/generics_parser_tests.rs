// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! Parsing of type parameters, constraints and instantiations.

use goiaba::parser::ast::{Decl, Expr, File, Spec, Stmt};
use goiaba::parser::objects::AstObjects;
use goiaba::parser::{Token, parse_str};

fn parse(source: &str) -> (AstObjects, File) {
    parse_str(source).unwrap_or_else(|e| panic!("should parse:\n{source}\n{e}"))
}

fn name(objs: &AstObjects, e: &Expr) -> String {
    match e {
        Expr::Ident(key) => objs.idents[*key].name.clone(),
        other => panic!("expected an identifier, found {other:?}"),
    }
}

/// The type parameters of a field list as `(names, constraint)` pairs.
fn params(objs: &AstObjects, list: &goiaba::parser::ast::FieldList) -> Vec<(Vec<String>, String)> {
    list.list
        .iter()
        .map(|key| {
            let field = &objs.fields[*key];
            let names = field
                .names
                .iter()
                .map(|n| objs.idents[*n].name.clone())
                .collect();
            (names, describe(objs, &field.typ))
        })
        .collect()
}

/// A compact rendering of a type or constraint expression.
fn describe(objs: &AstObjects, e: &Expr) -> String {
    match e {
        Expr::Ident(key) => objs.idents[*key].name.clone(),
        Expr::Selector(s) => format!("{}.{}", describe(objs, &s.expr), objs.idents[s.sel].name),
        Expr::Star(s) => format!("*{}", describe(objs, &s.expr)),
        Expr::Array(a) => match &a.len {
            None => format!("[]{}", describe(objs, &a.elt)),
            Some(len) => format!("[{}]{}", describe(objs, len), describe(objs, &a.elt)),
        },
        Expr::BasicLit(lit) => lit.token.get_literal().to_string(),
        Expr::Map(m) => format!("map[{}]{}", describe(objs, &m.key), describe(objs, &m.val)),
        Expr::Unary(u) if matches!(u.op, Token::TILDE) => format!("~{}", describe(objs, &u.expr)),
        Expr::Binary(b) if matches!(b.op, Token::OR) => {
            format!(
                "{} | {}",
                describe(objs, &b.expr_a),
                describe(objs, &b.expr_b)
            )
        }
        Expr::Binary(b) if matches!(b.op, Token::MUL) => {
            format!(
                "{}*{}",
                describe(objs, &b.expr_a),
                describe(objs, &b.expr_b)
            )
        }
        Expr::Index(ix) => format!(
            "{}[{}]",
            describe(objs, &ix.expr),
            describe(objs, &ix.index)
        ),
        Expr::IndexList(ix) => {
            let args: Vec<String> = ix.indices.iter().map(|a| describe(objs, a)).collect();
            format!("{}[{}]", describe(objs, &ix.expr), args.join(", "))
        }
        Expr::Interface(it) => {
            let parts: Vec<String> = it
                .methods
                .list
                .iter()
                .map(|key| {
                    let field = &objs.fields[*key];
                    match field.names.first() {
                        Some(n) => format!("{}()", objs.idents[*n].name),
                        None => describe(objs, &field.typ),
                    }
                })
                .collect();
            format!("interface{{{}}}", parts.join("; "))
        }
        Expr::Struct(_) => "struct".to_string(),
        other => format!("{other:?}"),
    }
}

fn func<'a>(objs: &'a AstObjects, file: &File, index: usize) -> &'a goiaba::parser::ast::FuncDecl {
    let funcs: Vec<_> = file
        .decls
        .iter()
        .filter_map(|d| match d {
            Decl::Func(key) => Some(&objs.fdecls[*key]),
            _ => None,
        })
        .collect();
    funcs[index]
}

fn type_specs<'a>(objs: &'a AstObjects, file: &File) -> Vec<&'a goiaba::parser::ast::TypeSpec> {
    let mut out = Vec::new();
    for decl in &file.decls {
        if let Decl::Gen(g) = decl {
            for key in &g.specs {
                if let Spec::Type(ts) = &objs.specs[*key] {
                    out.push(ts.as_ref());
                }
            }
        }
    }
    out
}

#[test]
fn generic_function_declarations() {
    let (objs, file) = parse(
        r#"
        package main

        func Map[T, U any](xs []T, f func(T) U) []U { return nil }
        func Max[T int | float64](a, b T) T { return a }
        func Sum[K comparable, V ~int | ~float64](m map[K]V) V { var s V; return s }
        func plain(x int) int { return x }
        "#,
    );
    let tp = |i: usize| {
        let ft = &objs.ftypes[func(&objs, &file, i).typ];
        ft.typ_params.as_ref().map(|list| params(&objs, list))
    };
    assert_eq!(
        tp(0),
        Some(vec![(
            vec!["T".to_string(), "U".to_string()],
            "any".to_string()
        )])
    );
    assert_eq!(
        tp(1),
        Some(vec![(vec!["T".to_string()], "int | float64".to_string())])
    );
    assert_eq!(
        tp(2),
        Some(vec![
            (vec!["K".to_string()], "comparable".to_string()),
            (vec!["V".to_string()], "~int | ~float64".to_string()),
        ])
    );
    assert_eq!(tp(3), None);
}

#[test]
fn generic_type_declarations_and_array_types() {
    let (objs, file) = parse(
        r#"
        package main

        const N = 4

        type List[T any] struct { items []T }
        type Pair[K comparable, V any] struct { key K; val V }
        type Tree[T interface{ ~int | ~string }] struct { left, right *Tree[T]; val T }
        type Grid [N]int
        type Scaled [N * 2]int
        type Bytes []byte
        type Fixed [8]byte
        type Alias = List[int]
        "#,
    );
    let specs = type_specs(&objs, &file);
    let tp = |i: usize| specs[i].typ_params.as_ref().map(|list| params(&objs, list));
    assert_eq!(
        tp(0),
        Some(vec![(vec!["T".to_string()], "any".to_string())])
    );
    assert_eq!(
        tp(1),
        Some(vec![
            (vec!["K".to_string()], "comparable".to_string()),
            (vec!["V".to_string()], "any".to_string()),
        ])
    );
    assert_eq!(
        tp(2),
        Some(vec![(
            vec!["T".to_string()],
            "interface{~int | ~string}".to_string()
        )])
    );
    // A bracket after the name is an array type when it holds a length.
    for i in 3..7 {
        assert_eq!(tp(i), None, "spec {i}");
    }
    assert_eq!(describe(&objs, &specs[3].typ), "[N]int");
    assert_eq!(describe(&objs, &specs[4].typ), "[N*2]int");
    assert_eq!(describe(&objs, &specs[5].typ), "[]byte");
    assert_eq!(describe(&objs, &specs[6].typ), "[8]byte");
    assert_eq!(describe(&objs, &specs[7].typ), "List[int]");
    assert!(specs[7].assign > 0);
}

#[test]
fn instantiated_types_in_declarations() {
    let (objs, file) = parse(
        r#"
        package main

        type Cache struct {
            entries  Pair[string, int]
            list     *List[int]
            data     [4]int
            names    []string
            matrix   [2][3]float64
            index    map[string]List[int]
            List[bool]
        }

        func (l *List[T]) Push(v T) {}
        func (p Pair[K, V]) Key() K { return p.key }
        func use(a [N]int, b List[int], c []List[string], d pkg.Set[int]) (List[int], error) { return b, nil }
        "#,
    );
    let specs = type_specs(&objs, &file);
    let Expr::Struct(st) = &specs[0].typ else {
        panic!("expected a struct");
    };
    let fields: Vec<(Vec<String>, String)> = params(&objs, &st.fields);
    let types: Vec<&str> = fields.iter().map(|(_, ty)| ty.as_str()).collect();
    assert_eq!(
        types,
        [
            "Pair[string, int]",
            "*List[int]",
            "[4]int",
            "[]string",
            "[2][3]float64",
            "map[string]List[int]",
            "List[bool]",
        ]
    );
    assert_eq!(fields[2].0, ["data"]);
    assert!(fields[6].0.is_empty(), "List[bool] is an embedded field");

    let recv = |i: usize| {
        let fd = func(&objs, &file, i);
        let list = fd.recv.as_ref().unwrap();
        describe(&objs, &objs.fields[list.list[0]].typ)
    };
    assert_eq!(recv(0), "*List[T]");
    assert_eq!(recv(1), "Pair[K, V]");

    let ft = &objs.ftypes[func(&objs, &file, 2).typ];
    let args = params(&objs, &ft.params);
    let arg_types: Vec<&str> = args.iter().map(|(_, ty)| ty.as_str()).collect();
    assert_eq!(
        arg_types,
        ["[N]int", "List[int]", "[]List[string]", "pkg.Set[int]"]
    );
    assert_eq!(args[0].0, ["a"]);
    let results = params(&objs, ft.results.as_ref().unwrap());
    assert_eq!(results[0].1, "List[int]");
}

#[test]
fn instantiation_in_expressions() {
    let (objs, file) = parse(
        r#"
        package main

        func main() {
            a := Map[int, string](xs, f)
            b := Max[float64](1, 2)
            c := List[int]{items: nil}
            d := Pair[string, []int]{"k", nil}
            e := xs[i]
            g := m["key"]
            h := xs[1:2]
            if xs[i] {
            }
            for _, v := range (List[int]{}) {
                _ = v
            }
            _, _, _, _, _, _, _ = a, b, c, d, e, g, h
        }
        "#,
    );
    let body = func(&objs, &file, 0).body.as_ref().unwrap();
    let rhs = |i: usize| match &body.list[i] {
        Stmt::Assign(key) => &objs.a_stmts[*key].rhs[0],
        other => panic!("expected an assignment, found {other:?}"),
    };
    let Expr::Call(call) = rhs(0) else {
        panic!("expected a call");
    };
    assert_eq!(describe(&objs, &call.func), "Map[int, string]");
    assert_eq!(call.args.len(), 2);

    let Expr::Call(call) = rhs(1) else {
        panic!("expected a call");
    };
    assert_eq!(describe(&objs, &call.func), "Max[float64]");

    let Expr::CompositeLit(lit) = rhs(2) else {
        panic!("expected a composite literal");
    };
    assert_eq!(describe(&objs, lit.typ.as_ref().unwrap()), "List[int]");
    let Expr::CompositeLit(lit) = rhs(3) else {
        panic!("expected a composite literal");
    };
    assert_eq!(
        describe(&objs, lit.typ.as_ref().unwrap()),
        "Pair[string, []int]"
    );
    assert_eq!(lit.elts.len(), 2);

    // Ordinary indexing and slicing are unchanged.
    let Expr::Index(ix) = rhs(4) else {
        panic!("expected an index expression");
    };
    assert_eq!(name(&objs, &ix.expr), "xs");
    assert!(matches!(rhs(5), Expr::Index(_)));
    assert!(matches!(rhs(6), Expr::Slice(_)));

    // In a condition, `xs[i] {` starts the block, not a composite literal.
    let Stmt::If(stmt) = &body.list[7] else {
        panic!("expected an if statement");
    };
    assert!(matches!(stmt.cond, Expr::Index(_)));

    let Stmt::Range(range) = &body.list[8] else {
        panic!("expected a range statement");
    };
    let Expr::Paren(paren) = &range.expr else {
        panic!("expected a parenthesized literal");
    };
    assert!(matches!(paren.expr, Expr::CompositeLit(_)));
}

#[test]
fn interfaces_with_type_elements() {
    let (objs, file) = parse(
        r#"
        package main

        type Number interface {
            ~int | ~int64 | ~float64
        }

        type Ordered interface {
            Number | ~string
            comparable
            String() string
        }

        type Reader interface {
            Read(p []byte) (int, error)
            io.Closer
        }
        "#,
    );
    let specs = type_specs(&objs, &file);
    assert_eq!(
        describe(&objs, &specs[0].typ),
        "interface{~int | ~int64 | ~float64}"
    );
    assert_eq!(
        describe(&objs, &specs[1].typ),
        "interface{Number | ~string; comparable; String()}"
    );
    assert_eq!(
        describe(&objs, &specs[2].typ),
        "interface{Read(); io.Closer}"
    );
}

#[test]
fn malformed_generics_are_errors() {
    assert!(parse_str("package main\nfunc F[T any(x T) {}\n").is_err());
    assert!(parse_str("package main\ntype T[P any struct{}\n").is_err());
    assert!(parse_str("package main\ntype I interface { ~ }\n").is_err());
}
