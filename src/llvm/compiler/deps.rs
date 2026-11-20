// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! Which package-level names an expression or function body refers to.
//! Package initialization order is derived from this.

use super::*;
use crate::parser::IdentKey;

/// The references found in a piece of syntax.
#[derive(Default)]
pub(super) struct Uses {
    /// Identifiers used as values. Field names and type syntax are left out.
    pub(super) idents: Vec<IdentKey>,
    /// Names selected with a dot: `x.name`. Some of them are method calls.
    pub(super) selected: Vec<String>,
}

pub(super) fn walk_expr(objs: &AstObjects, e: &Expr, uses: &mut Uses) {
    let each = |exprs: &[&Expr], uses: &mut Uses| {
        for e in exprs {
            walk_expr(objs, e, uses);
        }
    };
    match e {
        Expr::Ident(key) => uses.idents.push(*key),
        Expr::Ellipsis(e) => each(&e.elt.iter().collect::<Vec<_>>(), uses),
        Expr::FuncLit(lit) => walk_stmts(objs, &lit.body.list, uses),
        Expr::CompositeLit(lit) => {
            // In a struct literal a key is a field name, not a variable.
            let keys_are_values = matches!(&lit.typ, Some(Expr::Map(_)));
            for elt in &lit.elts {
                match elt {
                    Expr::KeyValue(kv) => {
                        if keys_are_values || !matches!(kv.key, Expr::Ident(_)) {
                            walk_expr(objs, &kv.key, uses);
                        }
                        walk_expr(objs, &kv.val, uses);
                    }
                    other => walk_expr(objs, other, uses),
                }
            }
        }
        Expr::Paren(p) => walk_expr(objs, &p.expr, uses),
        Expr::Selector(s) => {
            uses.selected.push(objs.idents[s.sel].name.clone());
            walk_expr(objs, &s.expr, uses);
        }
        Expr::Index(ix) => each(&[&ix.expr, &ix.index], uses),
        Expr::IndexList(ix) => walk_expr(objs, &ix.expr, uses),
        Expr::Slice(s) => {
            walk_expr(objs, &s.expr, uses);
            each(
                &[&s.low, &s.high, &s.max]
                    .into_iter()
                    .flatten()
                    .collect::<Vec<_>>(),
                uses,
            );
        }
        Expr::TypeAssert(ta) => walk_expr(objs, &ta.expr, uses),
        Expr::Call(call) => {
            walk_expr(objs, &call.func, uses);
            each(&call.args.iter().collect::<Vec<_>>(), uses);
        }
        Expr::Star(s) => walk_expr(objs, &s.expr, uses),
        Expr::Unary(u) => walk_expr(objs, &u.expr, uses),
        Expr::Binary(b) => each(&[&b.expr_a, &b.expr_b], uses),
        Expr::KeyValue(kv) => each(&[&kv.key, &kv.val], uses),
        // Literals and type syntax refer to no variables.
        Expr::Bad(_)
        | Expr::BasicLit(_)
        | Expr::Array(_)
        | Expr::Struct(_)
        | Expr::Func(_)
        | Expr::Interface(_)
        | Expr::Map(_)
        | Expr::Chan(_) => {}
    }
}

pub(super) fn walk_stmts(objs: &AstObjects, stmts: &[Stmt], uses: &mut Uses) {
    for s in stmts {
        walk_stmt(objs, s, uses);
    }
}

fn walk_stmt(objs: &AstObjects, s: &Stmt, uses: &mut Uses) {
    let opt = |s: &Option<Stmt>, uses: &mut Uses| {
        if let Some(s) = s {
            walk_stmt(objs, s, uses);
        }
    };
    match s {
        Stmt::Bad(_) | Stmt::Empty(_) | Stmt::Branch(_) => {}
        Stmt::Decl(decl) => {
            if let Decl::Gen(g) = decl.as_ref() {
                for key in &g.specs {
                    if let Spec::Value(vs) = &objs.specs[*key] {
                        for value in &vs.values {
                            walk_expr(objs, value, uses);
                        }
                    }
                }
            }
        }
        Stmt::Labeled(key) => walk_stmt(objs, &objs.l_stmts[*key].stmt, uses),
        Stmt::Expr(e) => walk_expr(objs, e, uses),
        Stmt::Send(send) => {
            walk_expr(objs, &send.chan, uses);
            walk_expr(objs, &send.val, uses);
        }
        Stmt::IncDec(inc) => walk_expr(objs, &inc.expr, uses),
        Stmt::Assign(key) => {
            let a = &objs.a_stmts[*key];
            for e in a.lhs.iter().chain(&a.rhs) {
                walk_expr(objs, e, uses);
            }
        }
        Stmt::Go(g) => walk_expr(objs, &g.call, uses),
        Stmt::Defer(d) => walk_expr(objs, &d.call, uses),
        Stmt::Return(ret) => {
            for e in &ret.results {
                walk_expr(objs, e, uses);
            }
        }
        Stmt::Block(block) => walk_stmts(objs, &block.list, uses),
        Stmt::If(stmt) => {
            opt(&stmt.init, uses);
            walk_expr(objs, &stmt.cond, uses);
            walk_stmts(objs, &stmt.body.list, uses);
            opt(&stmt.els, uses);
        }
        Stmt::Case(clause) => {
            for e in clause.list.iter().flatten() {
                walk_expr(objs, e, uses);
            }
            walk_stmts(objs, &clause.body, uses);
        }
        Stmt::Switch(stmt) => {
            opt(&stmt.init, uses);
            if let Some(tag) = &stmt.tag {
                walk_expr(objs, tag, uses);
            }
            walk_stmts(objs, &stmt.body.list, uses);
        }
        Stmt::TypeSwitch(stmt) => {
            opt(&stmt.init, uses);
            walk_stmt(objs, &stmt.assign, uses);
            // Case lists hold types; only the bodies matter.
            for clause in &stmt.body.list {
                if let Stmt::Case(clause) = clause {
                    walk_stmts(objs, &clause.body, uses);
                }
            }
        }
        Stmt::Comm(clause) => {
            opt(&clause.comm, uses);
            walk_stmts(objs, &clause.body, uses);
        }
        Stmt::Select(stmt) => walk_stmts(objs, &stmt.body.list, uses),
        Stmt::For(stmt) => {
            opt(&stmt.init, uses);
            if let Some(cond) = &stmt.cond {
                walk_expr(objs, cond, uses);
            }
            opt(&stmt.post, uses);
            walk_stmts(objs, &stmt.body.list, uses);
        }
        Stmt::Range(stmt) => {
            walk_expr(objs, &stmt.expr, uses);
            walk_stmts(objs, &stmt.body.list, uses);
        }
    }
}
