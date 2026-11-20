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

/// What a composite literal builds, which decides how its keys are read.
pub(super) enum Lit {
    /// A struct: the keys are field names.
    Struct,
    /// An array, slice or map with this element type: keys are expressions,
    /// and an element written without a type is a literal of that type.
    Seq(Ty),
    /// Not known, for example a type declared inside a function. Identifier
    /// keys are taken to be field names.
    Unknown,
}

/// Collects the references in expressions and statements.
pub(super) struct Walker<'w> {
    pub(super) objs: &'w AstObjects,
    /// Classifies a composite literal by the type written on it.
    pub(super) classify: &'w dyn Fn(&Expr) -> Lit,
    /// Classifies an element literal whose type is left out.
    pub(super) classify_elem: &'w dyn Fn(&Ty) -> Lit,
    pub(super) uses: Uses,
}

impl Walker<'_> {
    pub(super) fn expr(&mut self, e: &Expr) {
        match e {
            Expr::Ident(key) => self.uses.idents.push(*key),
            Expr::Ellipsis(e) => {
                if let Some(elt) = &e.elt {
                    self.expr(elt);
                }
            }
            Expr::FuncLit(lit) => self.stmts(&lit.body.list),
            Expr::CompositeLit(lit) => self.literal(lit, Lit::Unknown),
            Expr::Paren(p) => self.expr(&p.expr),
            Expr::Selector(s) => {
                self.uses
                    .selected
                    .push(self.objs.idents[s.sel].name.clone());
                self.expr(&s.expr);
            }
            Expr::Index(ix) => {
                self.expr(&ix.expr);
                self.expr(&ix.index);
            }
            Expr::IndexList(ix) => self.expr(&ix.expr),
            Expr::Slice(s) => {
                self.expr(&s.expr);
                for bound in [&s.low, &s.high, &s.max].into_iter().flatten() {
                    self.expr(bound);
                }
            }
            Expr::TypeAssert(ta) => self.expr(&ta.expr),
            Expr::Call(call) => {
                self.expr(&call.func);
                for arg in &call.args {
                    self.expr(arg);
                }
            }
            Expr::Star(s) => self.expr(&s.expr),
            Expr::Unary(u) => self.expr(&u.expr),
            Expr::Binary(b) => {
                self.expr(&b.expr_a);
                self.expr(&b.expr_b);
            }
            Expr::KeyValue(kv) => {
                self.expr(&kv.key);
                self.expr(&kv.val);
            }
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

    /// Walks a composite literal. `inherited` is its kind when it is an
    /// element written without a type, as in `[]Point{{x: 1}}`.
    fn literal(&mut self, lit: &ast::CompositeLit, inherited: Lit) {
        let kind = match &lit.typ {
            Some(typ) => (self.classify)(typ),
            None => inherited,
        };
        for elt in &lit.elts {
            let (key, val) = match elt {
                Expr::KeyValue(kv) => (Some(&kv.key), &kv.val),
                other => (None, other),
            };
            match &kind {
                Lit::Seq(elem) => {
                    if let Some(key) = key {
                        self.element(key, Lit::Unknown);
                    }
                    let elem = (self.classify_elem)(elem);
                    self.element(val, elem);
                }
                Lit::Struct | Lit::Unknown => {
                    if let Some(key) = key
                        && !matches!(key, Expr::Ident(_))
                    {
                        self.expr(key);
                    }
                    self.expr(val);
                }
            }
        }
    }

    fn element(&mut self, e: &Expr, kind: Lit) {
        match e {
            Expr::CompositeLit(lit) if lit.typ.is_none() => self.literal(lit, kind),
            _ => self.expr(e),
        }
    }

    pub(super) fn stmts(&mut self, stmts: &[Stmt]) {
        for s in stmts {
            self.stmt(s);
        }
    }

    fn opt(&mut self, s: &Option<Stmt>) {
        if let Some(s) = s {
            self.stmt(s);
        }
    }

    fn stmt(&mut self, s: &Stmt) {
        let objs = self.objs;
        match s {
            Stmt::Bad(_) | Stmt::Empty(_) | Stmt::Branch(_) => {}
            Stmt::Decl(decl) => {
                if let Decl::Gen(g) = decl.as_ref() {
                    for key in &g.specs {
                        if let Spec::Value(vs) = &objs.specs[*key] {
                            for value in &vs.values {
                                self.expr(value);
                            }
                        }
                    }
                }
            }
            Stmt::Labeled(key) => self.stmt(&objs.l_stmts[*key].stmt),
            Stmt::Expr(e) => self.expr(e),
            Stmt::Send(send) => {
                self.expr(&send.chan);
                self.expr(&send.val);
            }
            Stmt::IncDec(inc) => self.expr(&inc.expr),
            Stmt::Assign(key) => {
                let a = &objs.a_stmts[*key];
                for e in a.lhs.iter().chain(&a.rhs) {
                    self.expr(e);
                }
            }
            Stmt::Go(g) => self.expr(&g.call),
            Stmt::Defer(d) => self.expr(&d.call),
            Stmt::Return(ret) => {
                for e in &ret.results {
                    self.expr(e);
                }
            }
            Stmt::Block(block) => self.stmts(&block.list),
            Stmt::If(stmt) => {
                self.opt(&stmt.init);
                self.expr(&stmt.cond);
                self.stmts(&stmt.body.list);
                self.opt(&stmt.els);
            }
            Stmt::Case(clause) => {
                for e in clause.list.iter().flatten() {
                    self.expr(e);
                }
                self.stmts(&clause.body);
            }
            Stmt::Switch(stmt) => {
                self.opt(&stmt.init);
                if let Some(tag) = &stmt.tag {
                    self.expr(tag);
                }
                self.stmts(&stmt.body.list);
            }
            Stmt::TypeSwitch(stmt) => {
                self.opt(&stmt.init);
                self.stmt(&stmt.assign);
                // Case lists hold types; only the bodies matter.
                for clause in &stmt.body.list {
                    if let Stmt::Case(clause) = clause {
                        self.stmts(&clause.body);
                    }
                }
            }
            Stmt::Comm(clause) => {
                self.opt(&clause.comm);
                self.stmts(&clause.body);
            }
            Stmt::Select(stmt) => self.stmts(&stmt.body.list),
            Stmt::For(stmt) => {
                self.opt(&stmt.init);
                if let Some(cond) = &stmt.cond {
                    self.expr(cond);
                }
                self.opt(&stmt.post);
                self.stmts(&stmt.body.list);
            }
            Stmt::Range(stmt) => {
                // `for k, v = range x` assigns to existing variables.
                if matches!(stmt.token, Token::ASSIGN) {
                    for target in [&stmt.key, &stmt.val].into_iter().flatten() {
                        self.expr(target);
                    }
                }
                self.expr(&stmt.expr);
                self.stmts(&stmt.body.list);
            }
        }
    }
}
