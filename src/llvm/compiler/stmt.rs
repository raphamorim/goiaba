// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! Statements: declarations, assignment, control flow, range loops and select.

use super::*;

impl<'a> Compiler<'a> {
    pub(super) fn block(&mut self, stmts: &[Stmt]) -> R<()> {
        self.push_scope();
        let result = stmts.iter().try_for_each(|s| self.stmt(s));
        self.pop_scope();
        result
    }

    pub(super) fn stmt(&mut self, s: &Stmt) -> R<()> {
        let result = self.stmt_inner(s);
        result.map_err(|err| self.locate(s.pos(self.objs), err))
    }

    pub(super) fn stmt_inner(&mut self, s: &Stmt) -> R<()> {
        let objs = self.objs;
        match s {
            Stmt::Empty(_) => Ok(()),
            Stmt::Block(b) => self.block(&b.list),
            Stmt::Expr(e) => {
                let allowed = match strip_parens(e) {
                    Expr::Call(_) => true,
                    Expr::Unary(u) => matches!(u.op, Token::ARROW),
                    _ => false,
                };
                if !allowed {
                    return Err("expression statement must be a call or a receive".into());
                }
                self.expr(e).map(|_| ())
            }
            Stmt::Decl(decl) => self.local_decl(decl),
            Stmt::Assign(key) => self.assign(&objs.a_stmts[*key]),
            Stmt::IncDec(inc) => {
                let op = if matches!(inc.token, Token::INC) {
                    Op::Add
                } else {
                    Op::Sub
                };
                let target = self.target(&inc.expr)?;
                if target.ty().is_none() {
                    return Err("cannot increment _".into());
                }
                let cur = self.target_load(&target);
                let next = self.binary(op, cur, Val::int(Ty::UntypedInt, 1))?;
                self.target_store(&target, &next);
                Ok(())
            }
            Stmt::Return(ret) => self.ret(ret),
            Stmt::If(stmt) => self.if_stmt(stmt),
            Stmt::For(stmt) => self.for_stmt(stmt),
            Stmt::Range(stmt) => self.range_stmt(stmt),
            Stmt::Switch(stmt) => self.switch_stmt(stmt),
            Stmt::Branch(branch) => self.branch(branch),
            Stmt::Labeled(key) => {
                let labeled = &objs.l_stmts[*key];
                let label = objs.idents[labeled.label].name.clone();
                let depth = self.f.targets.len();
                let saved = self.f.pending_label.replace((label, depth));
                let result = self.stmt(&labeled.stmt);
                self.f.pending_label = saved;
                result
            }
            Stmt::Go(g) => {
                let Expr::Call(call) = strip_parens(&g.call) else {
                    return Err("go needs a function call".into());
                };
                let record = self.call_record(call, "go")?;
                self.emit(format!("call void @goiaba_go(ptr {record})"));
                Ok(())
            }
            Stmt::Send(send) => {
                let chan = self.expr(&send.chan)?;
                let Ty::Chan(elem) = chan.ty.under().clone() else {
                    return Err(format!("cannot send to {}", chan.ty));
                };
                let val = self.expr_want(&send.val, &elem)?;
                let slot = self.spill(&val);
                self.emit(format!(
                    "call void @goiaba_chan_send(ptr {}, ptr {slot})",
                    chan.repr
                ));
                Ok(())
            }
            Stmt::Select(sel) => self.select_stmt(sel),
            Stmt::Defer(d) => self.defer_stmt(d),
            Stmt::Comm(_) => Err("communication clause outside of a select".into()),
            Stmt::TypeSwitch(ts) => self.type_switch(ts),
            Stmt::Case(_) => Err("case clause outside of a switch".into()),
            Stmt::Bad(_) => Err("invalid statement".into()),
        }
    }

    pub(super) fn local_decl(&mut self, decl: &Decl) -> R<()> {
        let objs = self.objs;
        let Decl::Gen(g) = decl else {
            return Err("unsupported local declaration".into());
        };
        if matches!(g.token, Token::CONST) {
            return self.const_group(&g.specs);
        }
        for key in &g.specs {
            match &objs.specs[*key] {
                Spec::Type(ts) => {
                    // A local type gets a unique key and is bound in the
                    // enclosing scope, so the same name can be declared in
                    // several functions or shadow a package-level type.
                    let name = objs.idents[ts.name].name.as_str();
                    self.local_types += 1;
                    let key = self.q(&format!("{name}${}", self.local_types));
                    let ty = if let Expr::Struct(st) = &ts.typ {
                        self.declare_struct_name(&key);
                        // Bound first, so the struct can refer to itself.
                        self.bind(name, Binding::Type(Ty::Struct(key.clone())));
                        self.define_struct(&key, st)
                            .map_err(|e| e.replace(&key, name))?;
                        Ty::Struct(key)
                    } else if ts.assign > 0 {
                        self.resolve_type(&ts.typ)?
                    } else {
                        // Bound first, so the type can refer to itself.
                        let cell = Under::pending();
                        let ty = Ty::Named(key.clone(), cell.clone());
                        self.bind(name, Binding::Type(ty.clone()));
                        let under = self.resolve_type(&ts.typ)?;
                        if self.contains_by_value(&under, &key, &mut Vec::new()) {
                            return Err(format!("invalid recursive type {name}"));
                        }
                        cell.set(under);
                        ty
                    };
                    self.bind(name, Binding::Type(ty));
                }
                Spec::Value(vs) => self.var_spec(vs)?,
                Spec::Import(_) => return Err("import inside a function".into()),
            }
        }
        Ok(())
    }

    pub(super) fn var_spec(&mut self, vs: &ast::ValueSpec) -> R<()> {
        let objs = self.objs;
        let want = match &vs.typ {
            Some(t) => Some(self.resolve_type(t)?),
            None => None,
        };
        let names: Vec<&str> = vs
            .names
            .iter()
            .map(|n| objs.idents[*n].name.as_str())
            .collect();

        if vs.values.is_empty() {
            let ty = want.ok_or("var declaration needs a type or a value")?;
            for name in names {
                self.declare(name, ty.clone(), None);
            }
            return Ok(());
        }
        let mut vals = self.eval_rhs(&vs.values, names.len(), want.as_ref())?;
        for (name, val) in names.into_iter().zip(vals.drain(..)) {
            let val = match &want {
                Some(ty) => self.coerce(val, ty)?,
                None => self.materialize(val)?,
            };
            self.declare(name, val.ty.clone(), Some(val));
        }
        Ok(())
    }

    /// Evaluates the right side of an assignment to exactly `n` values.
    /// A single call returning a tuple is spread over the targets.
    pub(super) fn eval_rhs(&mut self, rhs: &[Expr], n: usize, want: Option<&Ty>) -> R<Vec<Val>> {
        if rhs.len() == n {
            return rhs
                .iter()
                .map(|e| match want {
                    Some(ty) => self.expr_want(e, ty),
                    None => self.expr(e),
                })
                .collect();
        }
        if rhs.len() == 1 {
            let val = match strip_parens(&rhs[0]) {
                // `v, ok := x.(T)` reports failure instead of panicking.
                Expr::TypeAssert(ta) if n == 2 => self.type_assert(ta, true)?,
                // `v, ok := m[k]` reports whether the key is present.
                // `v, ok := <-ch` reports whether the channel is still open.
                Expr::Unary(u) if n == 2 && matches!(u.op, Token::ARROW) => {
                    let (val, ok) = self.chan_recv(&u.expr)?;
                    let tuple = Ty::Tuple(vec![val.ty.clone(), Ty::Bool]);
                    let ll = tuple.ll();
                    let a = self.ins(format!(
                        "insertvalue {ll} poison, {} {}, 0",
                        val.ty.ll(),
                        val.repr
                    ));
                    let b = self.ins(format!("insertvalue {ll} {a}, i1 {ok}, 1"));
                    Val::new(tuple, b)
                }
                Expr::Index(ix) if n == 2 => {
                    let map = self.expr(&ix.expr)?;
                    let Ty::Map(key, val) = map.ty.under().clone() else {
                        return Err(format!("assignment mismatch: {n} variables but 1 value"));
                    };
                    self.map_get(&map.repr, &key, &val, &ix.index, true)?
                }
                _ => self.expr(&rhs[0])?,
            };
            if let Ty::Tuple(items) = &val.ty
                && items.len() == n
            {
                let ll = val.ty.ll();
                return Ok(items
                    .iter()
                    .enumerate()
                    .map(|(i, ty)| {
                        let name = self.ins(format!("extractvalue {ll} {}, {i}", val.repr));
                        Val::new(ty.clone(), name)
                    })
                    .collect());
            }
        }
        Err(format!(
            "assignment mismatch: {n} variables but {} values",
            rhs.len()
        ))
    }

    pub(super) fn assign(&mut self, a: &ast::AssignStmt) -> R<()> {
        let objs = self.objs;
        if let Some(op) = Op::from_assign(&a.token) {
            let ([lhs], [rhs]) = (a.lhs.as_slice(), a.rhs.as_slice()) else {
                return Err("compound assignment takes one operand on each side".into());
            };
            let target = self.target(lhs)?;
            let Some(ty) = target.ty() else {
                return Err("cannot assign to _".into());
            };
            let cur = self.target_load(&target);
            let right = self.expr(rhs)?;
            let result = self.binary(op, cur, right)?;
            let result = self.coerce(result, &ty)?;
            self.target_store(&target, &result);
            return Ok(());
        }

        let define = match a.token {
            Token::DEFINE => true,
            Token::ASSIGN => false,
            _ => return Err(format!("unsupported assignment operator {:?}", a.token)),
        };

        // A single typed target lets an untyped literal on the right pick up its type.
        if !define && a.lhs.len() == 1 && a.rhs.len() == 1 {
            let target = self.target(&a.lhs[0])?;
            let Some(ty) = target.ty() else {
                return self.expr(&a.rhs[0]).map(|_| ());
            };
            let val = self.expr_want(&a.rhs[0], &ty)?;
            self.target_store(&target, &val);
            return Ok(());
        }

        // Go's order: the index and pointer operands on the left, then the
        // right side, then the assignments from left to right.
        let is_new = |c: &Self, lhs: &Expr| match strip_parens(lhs) {
            Expr::Ident(key) if define => {
                let name = objs.idents[*key].name.as_str();
                name != "_" && !c.f.scopes.last().is_some_and(|s| s.contains_key(name))
            }
            _ => false,
        };
        let mut targets = Vec::new();
        for lhs in &a.lhs {
            targets.push(if is_new(self, lhs) {
                None
            } else {
                Some(self.target(lhs)?)
            });
        }
        let vals = self.eval_rhs(&a.rhs, a.lhs.len(), None)?;
        for ((lhs, target), val) in a.lhs.iter().zip(targets).zip(vals) {
            match target {
                None => {
                    let Expr::Ident(key) = strip_parens(lhs) else {
                        return Err("internal: new variable is not a name".into());
                    };
                    let val = self.materialize(val)?;
                    self.declare(&objs.idents[*key].name, val.ty.clone(), Some(val));
                }
                Some(target) => {
                    if let Some(ty) = target.ty() {
                        let val = self.coerce(val, &ty)?;
                        self.target_store(&target, &val);
                    }
                }
            }
        }
        Ok(())
    }

    pub(super) fn ret(&mut self, ret: &ast::ReturnStmt) -> R<()> {
        let want = self.f.ret.clone();
        let types = match &want {
            Ty::Void => Vec::new(),
            Ty::Tuple(items) => items.clone(),
            other => vec![other.clone()],
        };
        let vals: Vec<Val> = if ret.results.is_empty() && !types.is_empty() {
            // A bare return reads the named results.
            let names = self.f.named_results.clone();
            if names.is_empty() {
                return Err("missing return value".into());
            }
            names
                .iter()
                .map(|name| match self.lookup(name) {
                    Some(Binding::Var { ptr, ty, .. }) => Ok(self.load(&ptr, &ty)),
                    _ => Err(format!("internal: result {name} is not a variable")),
                })
                .collect::<R<_>>()?
        } else if ret.results.len() == types.len() {
            ret.results
                .iter()
                .zip(&types)
                .map(|(e, ty)| self.expr_want(e, ty))
                .collect::<R<_>>()?
        } else if ret.results.len() == 1 && types.len() > 1 {
            // `return f()` where f returns the same tuple.
            let val = self.expr(&ret.results[0])?;
            if val.ty != want {
                return Err(format!("cannot return {} as {want}", val.ty));
            }
            let ll = want.ll();
            types
                .iter()
                .enumerate()
                .map(|(i, ty)| {
                    let name = self.ins(format!("extractvalue {ll} {}, {i}", val.repr));
                    Val::new(ty.clone(), name)
                })
                .collect()
        } else {
            return Err(format!(
                "wrong number of return values: want {}, got {}",
                types.len(),
                ret.results.len()
            ));
        };

        if self.f.uses_defer {
            // Store the results, then leave through the block that runs the defers.
            let slots = self.f.ret_slots.clone();
            for (val, (slot, _)) in vals.iter().zip(&slots) {
                self.emit(format!(
                    "store volatile {} {}, ptr {slot}",
                    val.ty.ll(),
                    val.repr
                ));
            }
            self.br("fn.exit");
            return Ok(());
        }
        self.emit_ret(&want, &vals);
        Ok(())
    }

    pub(super) fn if_stmt(&mut self, stmt: &ast::IfStmt) -> R<()> {
        self.push_scope();
        let result = (|| {
            if let Some(init) = &stmt.init {
                self.stmt(init)?;
            }
            let cond = self.expr_want(&stmt.cond, &Ty::Bool)?;
            let then = self.new_label("if.then");
            let end = self.new_label("if.end");
            let els = match &stmt.els {
                Some(_) => self.new_label("if.else"),
                None => end.clone(),
            };
            self.cond_br(&cond.repr, &then, &els);
            self.place_label(&then);
            self.block(&stmt.body.list)?;
            self.br(&end);
            if let Some(else_stmt) = &stmt.els {
                self.place_label(&els);
                self.stmt(else_stmt)?;
                self.br(&end);
            }
            self.place_label(&end);
            Ok(())
        })();
        self.pop_scope();
        result
    }

    pub(super) fn for_stmt(&mut self, stmt: &ast::ForStmt) -> R<()> {
        let objs = self.objs;
        self.push_scope();
        let result = (|| {
            if let Some(init) = &stmt.init {
                self.stmt(init)?;
            }
            // Each iteration has its own copy of a loop variable. That is only
            // observable when the variable's address escapes, so only those
            // variables get a slot that is repointed before every post statement.
            let mut per_iteration = Vec::new();
            if let Some(Stmt::Assign(key)) = &stmt.init
                && matches!(objs.a_stmts[*key].token, Token::DEFINE)
            {
                for lhs in &objs.a_stmts[*key].lhs {
                    let Expr::Ident(ident) = lhs else { continue };
                    let name = objs.idents[*ident].name.as_str();
                    let scope = self.f.scopes.last().and_then(|s| s.get(name)).cloned();
                    if let Some(Binding::Var {
                        ptr,
                        ty,
                        heap: true,
                        indirect: false,
                    }) = scope
                    {
                        let slot = self.alloca(&Ty::Nil);
                        self.emit(format!("store ptr {ptr}, ptr {slot}"));
                        self.bind(
                            name,
                            Binding::Var {
                                ptr: slot.clone(),
                                ty: ty.clone(),
                                heap: true,
                                indirect: true,
                            },
                        );
                        per_iteration.push((slot, ty));
                    }
                }
            }

            let cond = self.new_label("for.cond");
            let body = self.new_label("for.body");
            let post = self.new_label("for.post");
            let end = self.new_label("for.end");
            self.place_label(&cond);
            if let Some(e) = &stmt.cond {
                let val = self.expr_want(e, &Ty::Bool)?;
                self.cond_br(&val.repr, &body, &end);
            } else {
                self.br(&body);
            }
            self.place_label(&body);
            self.push_target(end.clone(), Some(post.clone()));
            let body_result = self.block(&stmt.body.list);
            self.f.targets.pop();
            body_result?;
            self.place_label(&post);
            for (slot, ty) in &per_iteration {
                let old = self.ins(format!("load ptr, ptr {slot}"));
                let val = self.load(&old, ty);
                let fresh = self.heap_alloc(ty);
                self.store(&val, &fresh);
                self.emit(format!("store ptr {fresh}, ptr {slot}"));
            }
            if let Some(post_stmt) = &stmt.post {
                self.stmt(post_stmt)?;
            }
            self.br(&cond);
            self.place_label(&end);
            Ok(())
        })();
        self.pop_scope();
        result
    }

    pub(super) fn range_stmt(&mut self, stmt: &ast::RangeStmt) -> R<()> {
        /// What a range loop walks over.
        enum Subject {
            Count,
            /// First element's address and the element type.
            Elems(String, Ty),
            /// The string's bytes; iteration yields runes.
            Runes(String),
        }
        self.push_scope();
        let result = (|| {
            let subject = self.expr_m(&stmt.expr)?;
            if let Ty::Chan(elem) = subject.ty.under().clone() {
                return self.range_chan(stmt, &subject.repr, &elem);
            }
            if let Ty::Map(key, val) = subject.ty.under().clone() {
                return self.range_map(stmt, &subject.repr, &key, &val);
            }
            let (len, kind) = match subject.ty.under() {
                Ty::Int { .. } => (self.widen_i64(subject.clone())?, Subject::Count),
                Ty::Slice(elem) => {
                    let elem = elem.as_ref().clone();
                    let data = self.ins(format!("extractvalue %slice {}, 0", subject.repr));
                    let len = self.ins(format!("extractvalue %slice {}, 1", subject.repr));
                    (len, Subject::Elems(data, elem))
                }
                Ty::Array(elem, n) => {
                    let (elem, n) = (elem.as_ref().clone(), *n);
                    let slot = self.spill(&subject);
                    (n.to_string(), Subject::Elems(slot, elem))
                }
                Ty::Str => {
                    let data = self.ins(format!("extractvalue %string {}, 0", subject.repr));
                    let len = self.ins(format!("extractvalue %string {}, 1", subject.repr));
                    (len, Subject::Runes(data))
                }
                _ => return Err(format!("cannot range over {} yet", subject.ty)),
            };
            let define = matches!(stmt.token, Token::DEFINE);
            let counter = self.alloca(&INT);
            self.store(&Val::int(INT, 0), &counter);
            // Bytes consumed by the current rune when ranging over a string.
            let width = self.alloca(&INT);
            self.store(&Val::int(INT, 1), &width);

            let cond = self.new_label("range.cond");
            let body = self.new_label("range.body");
            let post = self.new_label("range.post");
            let end = self.new_label("range.end");
            self.place_label(&cond);
            let i = self.load(&counter, &INT);
            let more = self.ins(format!("icmp slt i64 {}, {len}", i.repr));
            self.cond_br(&more, &body, &end);

            self.place_label(&body);
            self.push_scope();
            let body_result = (|| {
                let item = match &kind {
                    Subject::Count => None,
                    Subject::Elems(data, elem) => {
                        let slot = self.ins(format!(
                            "getelementptr inbounds {}, ptr {data}, i64 {}",
                            elem.ll(),
                            i.repr
                        ));
                        Some(self.load(&slot, elem))
                    }
                    Subject::Runes(data) => {
                        let rune = self.ins(format!(
                            "call i32 @goiaba_decode_rune(ptr {data}, i64 {len}, i64 {}, ptr {width})",
                            i.repr
                        ));
                        Some(Val::new(
                            Ty::Int {
                                bits: 32,
                                signed: true,
                            },
                            rune,
                        ))
                    }
                };
                if let Some(key) = &stmt.key {
                    self.range_bind(key, i.clone(), define)?;
                }
                if let Some(value) = &stmt.val {
                    let Some(item) = item else {
                        return Err("range over an integer yields only one value".to_string());
                    };
                    self.range_bind(value, item, define)?;
                }
                self.push_target(end.clone(), Some(post.clone()));
                let r = self.block(&stmt.body.list);
                self.f.targets.pop();
                r
            })();
            self.pop_scope();
            body_result?;

            self.place_label(&post);
            let cur = self.load(&counter, &INT);
            let step = self.load(&width, &INT);
            let next = self.ins(format!("add i64 {}, {}", cur.repr, step.repr));
            self.store(&Val::new(INT, next), &counter);
            self.br(&cond);
            self.place_label(&end);
            Ok(())
        })();
        self.pop_scope();
        result
    }

    /// `for k, v := range m`. Entries come in insertion order.
    pub(super) fn range_map(
        &mut self,
        stmt: &ast::RangeStmt,
        map: &str,
        key: &Ty,
        val: &Ty,
    ) -> R<()> {
        let define = matches!(stmt.token, Token::DEFINE);
        let cursor = self.alloca(&INT);
        self.store(&Val::int(INT, 0), &cursor);
        let key_out = self.alloca(&Ty::Nil);
        let val_out = self.alloca(&Ty::Nil);

        let cond = self.new_label("range.cond");
        let body = self.new_label("range.body");
        let end = self.new_label("range.end");
        self.place_label(&cond);
        let more = self.ins(format!(
            "call i32 @goiaba_map_next(ptr {map}, ptr {cursor}, ptr {key_out}, ptr {val_out})"
        ));
        let more = self.ins(format!("icmp ne i32 {more}, 0"));
        self.cond_br(&more, &body, &end);

        self.place_label(&body);
        self.push_scope();
        let result = (|| {
            if let Some(target) = &stmt.key {
                let slot = self.ins(format!("load ptr, ptr {key_out}"));
                let k = self.load(&slot, key);
                self.range_bind(target, k, define)?;
            }
            if let Some(target) = &stmt.val {
                let slot = self.ins(format!("load ptr, ptr {val_out}"));
                let v = self.load(&slot, val);
                self.range_bind(target, v, define)?;
            }
            self.push_target(end.clone(), Some(cond.clone()));
            let r = self.block(&stmt.body.list);
            self.f.targets.pop();
            r
        })();
        self.pop_scope();
        result?;
        self.br(&cond);
        self.place_label(&end);
        Ok(())
    }

    /// `<-ch`: returns the received value and the i1 that is false once the
    /// channel is closed and drained.
    pub(super) fn chan_recv(&mut self, chan: &Expr) -> R<(Val, String)> {
        let chan = self.expr(chan)?;
        let Ty::Chan(elem) = chan.ty.under().clone() else {
            return Err(format!("cannot receive from {}", chan.ty));
        };
        let slot = self.alloca(&elem);
        let ok = self.ins(format!(
            "call i32 @goiaba_chan_recv(ptr {}, ptr {slot})",
            chan.repr
        ));
        let ok = self.ins(format!("icmp ne i32 {ok}, 0"));
        Ok((self.load(&slot, &elem), ok))
    }

    /// `for v := range ch`: receives until the channel is closed.
    pub(super) fn range_chan(&mut self, stmt: &ast::RangeStmt, chan: &str, elem: &Ty) -> R<()> {
        let define = matches!(stmt.token, Token::DEFINE);
        let slot = self.alloca(elem);
        let cond = self.new_label("range.cond");
        let body = self.new_label("range.body");
        let end = self.new_label("range.end");
        self.place_label(&cond);
        let ok = self.ins(format!(
            "call i32 @goiaba_chan_recv(ptr {chan}, ptr {slot})"
        ));
        let ok = self.ins(format!("icmp ne i32 {ok}, 0"));
        self.cond_br(&ok, &body, &end);

        self.place_label(&body);
        self.push_scope();
        let result = (|| {
            if let Some(target) = &stmt.key {
                let v = self.load(&slot, elem);
                self.range_bind(target, v, define)?;
            }
            self.push_target(end.clone(), Some(cond.clone()));
            let r = self.block(&stmt.body.list);
            self.f.targets.pop();
            r
        })();
        self.pop_scope();
        result?;
        self.br(&cond);
        self.place_label(&end);
        Ok(())
    }

    /// `select`: every case's channel and value are evaluated, the runtime
    /// picks a case that can proceed (or blocks), and control jumps to it.
    pub(super) fn select_stmt(&mut self, sel: &ast::SelectStmt) -> R<()> {
        let objs = self.objs;
        /// What a receive case does with the value it gets.
        struct Recv<'s> {
            slot: String,
            elem: Ty,
            targets: &'s [Expr],
            define: bool,
        }
        let mut clauses = Vec::new();
        let mut default = None;
        for s in &sel.body.list {
            let Stmt::Comm(clause) = s else {
                return Err("unexpected statement in select body".into());
            };
            let label = self.new_label("select.case");
            match &clause.comm {
                None => default = Some((clause, label)),
                Some(comm) => clauses.push((clause, comm, label)),
            }
        }

        // The case table: { channel, is-send, element address } per case.
        let n = clauses.len();
        let table = format!("%a{}", self.fresh());
        self.f.allocas.push(format!(
            "  {table} = alloca [{} x {{ ptr, i64, ptr }}]",
            n.max(1)
        ));
        let mut recvs = Vec::new();
        for (i, (_, comm, _)) in clauses.iter().enumerate() {
            let (chan, is_send, slot, recv) = match comm {
                Stmt::Send(send) => {
                    let chan = self.expr(&send.chan)?;
                    let Ty::Chan(elem) = chan.ty.under().clone() else {
                        return Err(format!("cannot send to {}", chan.ty));
                    };
                    let val = self.expr_want(&send.val, &elem)?;
                    (chan, 1, self.spill(&val), None)
                }
                other => {
                    // `<-ch`, `v := <-ch`, `v, ok := <-ch` or the `=` forms.
                    let (source, targets, define): (&Expr, &[Expr], bool) = match other {
                        Stmt::Expr(e) => (e.as_ref(), &[], false),
                        Stmt::Assign(key) => {
                            let a = &objs.a_stmts[*key];
                            let [source] = a.rhs.as_slice() else {
                                return Err("invalid select case".into());
                            };
                            (source, a.lhs.as_slice(), matches!(a.token, Token::DEFINE))
                        }
                        _ => return Err("invalid select case".into()),
                    };
                    let Expr::Unary(u) = strip_parens(source) else {
                        return Err("select case must be a send or a receive".into());
                    };
                    if !matches!(u.op, Token::ARROW) {
                        return Err("select case must be a send or a receive".into());
                    }
                    let chan = self.expr(&u.expr)?;
                    let Ty::Chan(elem) = chan.ty.under().clone() else {
                        return Err(format!("cannot receive from {}", chan.ty));
                    };
                    let slot = self.alloca(&elem);
                    let recv = Recv {
                        slot: slot.clone(),
                        elem: *elem,
                        targets,
                        define,
                    };
                    (chan, 0, slot, Some(recv))
                }
            };
            recvs.push(recv);
            let entry = self.ins(format!(
                "getelementptr inbounds [{} x {{ ptr, i64, ptr }}], ptr {table}, i64 0, i64 {i}",
                n.max(1)
            ));
            let a = self.ins(format!(
                "insertvalue {{ ptr, i64, ptr }} poison, ptr {}, 0",
                chan.repr
            ));
            let b = self.ins(format!(
                "insertvalue {{ ptr, i64, ptr }} {a}, i64 {is_send}, 1"
            ));
            let c = self.ins(format!(
                "insertvalue {{ ptr, i64, ptr }} {b}, ptr {slot}, 2"
            ));
            self.emit(format!("store {{ ptr, i64, ptr }} {c}, ptr {entry}"));
        }

        let ok_slot = self.alloca(&Ty::Int {
            bits: 32,
            signed: true,
        });
        let chosen = self.ins(format!(
            "call i64 @goiaba_select(ptr {table}, i64 {n}, i32 {}, ptr {ok_slot})",
            default.is_some() as i32
        ));
        let end = self.new_label("select.end");
        for (i, (_, _, label)) in clauses.iter().enumerate() {
            let hit = self.ins(format!("icmp eq i64 {chosen}, {i}"));
            let next = self.new_label("select.next");
            self.cond_br(&hit, label, &next);
            self.place_label(&next);
        }
        match &default {
            Some((_, label)) => self.br(label),
            // Without a default the runtime always returns a case index.
            None => {
                self.emit("unreachable".into());
                self.f.terminated = true;
            }
        }

        let bodies = clauses
            .iter()
            .zip(recvs)
            .map(|((clause, _, label), recv)| (*clause, label, recv))
            .chain(default.iter().map(|(clause, label)| (*clause, label, None)));
        for (clause, label, recv) in bodies {
            self.place_label(label);
            self.push_scope();
            let result = (|| {
                if let Some(recv) = recv {
                    let val = self.load(&recv.slot, &recv.elem);
                    if let Some(target) = recv.targets.first() {
                        self.range_bind(target, val, recv.define)?;
                    }
                    if let Some(target) = recv.targets.get(1) {
                        let ok = self.ins(format!("load i32, ptr {ok_slot}"));
                        let ok = self.ins(format!("icmp ne i32 {ok}, 0"));
                        self.range_bind(target, Val::new(Ty::Bool, ok), recv.define)?;
                    }
                }
                self.push_target(end.clone(), None);
                let r = self.block(&clause.body);
                self.f.targets.pop();
                r
            })();
            self.pop_scope();
            result?;
            self.br(&end);
        }
        self.place_label(&end);
        Ok(())
    }

    pub(super) fn range_bind(&mut self, target: &Expr, val: Val, define: bool) -> R<()> {
        let objs = self.objs;
        if let Expr::Ident(key) = strip_parens(target) {
            let name = objs.idents[*key].name.as_str();
            if name == "_" {
                return Ok(());
            }
            if define {
                self.declare(name, val.ty.clone(), Some(val));
                return Ok(());
            }
        }
        let target = self.target(target)?;
        if let Some(ty) = target.ty() {
            let val = self.coerce(val, &ty)?;
            self.target_store(&target, &val);
        }
        Ok(())
    }

    pub(super) fn switch_stmt(&mut self, stmt: &ast::SwitchStmt) -> R<()> {
        self.push_scope();
        let result = (|| {
            if let Some(init) = &stmt.init {
                self.stmt(init)?;
            }
            let tag = match &stmt.tag {
                Some(e) => Some(self.expr_m(e)?),
                None => None,
            };
            let end = self.new_label("switch.end");
            let mut clauses = Vec::new();
            for s in &stmt.body.list {
                let Stmt::Case(clause) = s else {
                    return Err("unexpected statement in switch body".to_string());
                };
                clauses.push((clause, self.new_label("switch.case")));
            }

            // Tests run in source order; `default` is taken only when none match.
            let mut default = None;
            for (clause, label) in &clauses {
                let Some(exprs) = clause.list.as_ref().filter(|l| !l.is_empty()) else {
                    default = Some(label.clone());
                    continue;
                };
                for e in exprs {
                    let hit = match &tag {
                        Some(tag) => {
                            let val = self.expr(e)?;
                            self.binary(Op::Eq, tag.clone(), val)?
                        }
                        None => self.expr_want(e, &Ty::Bool)?,
                    };
                    let next = self.new_label("switch.next");
                    self.cond_br(&hit.repr, label, &next);
                    self.place_label(&next);
                }
            }
            self.br(default.as_deref().unwrap_or(&end));

            for (clause, label) in &clauses {
                self.place_label(label);
                self.push_target(end.clone(), None);
                let r = self.block(&clause.body);
                self.f.targets.pop();
                r?;
                self.br(&end);
            }
            self.place_label(&end);
            Ok(())
        })();
        self.pop_scope();
        result
    }

    /// Enters a statement that `break` or `continue` can target. It takes the
    /// pending label when it is the labeled statement itself. A switch pushes
    /// once per clause, at the same depth, so every clause gets the label.
    pub(super) fn push_target(&mut self, brk: String, cont: Option<String>) {
        let depth = self.f.targets.len();
        let label = match &self.f.pending_label {
            Some((label, at)) if *at == depth => Some(label.clone()),
            _ => None,
        };
        self.f.targets.push(Target { brk, cont, label });
    }

    pub(super) fn branch(&mut self, branch: &ast::BranchStmt) -> R<()> {
        let label = branch.label.map(|key| self.objs.idents[key].name.clone());
        let targets = self.f.targets.iter().rev();
        let target = match (&branch.token, &label) {
            (Token::BREAK, None) => self.f.targets.last().map(|t| t.brk.clone()),
            (Token::BREAK, Some(_)) => targets
                .filter(|t| t.label == label)
                .map(|t| t.brk.clone())
                .next(),
            (Token::CONTINUE, None) => targets.filter_map(|t| t.cont.clone()).next(),
            (Token::CONTINUE, Some(_)) => targets
                .filter(|t| t.label == label)
                .filter_map(|t| t.cont.clone())
                .next(),
            (Token::FALLTHROUGH, _) => return Err("fallthrough is not supported yet".into()),
            _ => return Err("goto is not supported yet".into()),
        };
        let Some(target) = target else {
            return Err(match label {
                Some(label) => format!("invalid break or continue label {label}"),
                None => "break or continue outside of a loop".into(),
            });
        };
        self.br(&target);
        Ok(())
    }
}
