// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! Interfaces: method sets, run-time type descriptors, method tables, assertions and type switches.

use super::*;

impl<'a> Compiler<'a> {
    /// The methods callable on a value of type `ty`, sorted by name: its own
    /// and the ones promoted from embedded fields. A value of type T has the
    /// value-receiver methods; *T has all of them. Each entry carries the
    /// field path to the embedded value that declares the method.
    pub(super) fn method_set(&self, ty: &Ty) -> Vec<(String, FuncSig, Vec<usize>)> {
        let (base, is_ptr) = match ty {
            Ty::Ptr(inner) => (inner.as_ref(), true),
            other => (other, false),
        };
        let mut set = Vec::new();
        self.collect_methods(base, is_ptr, Vec::new(), &mut set, 0);
        set.sort_by(|a, b| a.0.cmp(&b.0));
        set
    }

    pub(super) fn collect_methods(
        &self,
        base: &Ty,
        all: bool,
        path: Vec<usize>,
        set: &mut Vec<(String, FuncSig, Vec<usize>)>,
        depth: usize,
    ) {
        if depth > 8 {
            return;
        }
        if let Some(type_name) = base.type_name() {
            for method in self.methods.get(type_name).into_iter().flatten() {
                let sig = &self.funcs[&format!("{type_name}.{method}")];
                let visible = all || sig.ptr_recv == Some(false);
                if visible && !set.iter().any(|(name, ..)| name == method) {
                    set.push((method.clone(), sig.clone(), path.clone()));
                }
            }
        }
        let Ty::Struct(struct_name) = base.under() else {
            return;
        };
        let Some(fields) = self.structs.get(struct_name) else {
            return;
        };
        for index in self.embedded.get(struct_name).into_iter().flatten() {
            let (inner, through_ptr) = match &fields[*index].1 {
                Ty::Ptr(inner) => (inner.as_ref(), true),
                other => (other, false),
            };
            let mut next = path.clone();
            next.push(*index);
            self.collect_methods(inner, all || through_ptr, next, set, depth + 1);
        }
    }

    /// The interface-callable function for a method of `ty`. A promoted
    /// method gets a wrapper that steps to the embedded value first.
    pub(super) fn method_fn(
        &mut self,
        ty: &Ty,
        method: &str,
        sig: &FuncSig,
        path: &[usize],
    ) -> String {
        let inner = self.iface_fn(sig);
        if path.is_empty() {
            return inner;
        }
        let base = match ty {
            Ty::Ptr(inner) => inner.as_ref(),
            other => other,
        };
        let wrapper = format!("{}.{method}$embed", base.go_name());
        if self.thunks.insert(wrapper.clone()) {
            let mut params = vec!["ptr %data".to_string()];
            let mut args = Vec::new();
            for (i, param) in sig.params.iter().enumerate().skip(1) {
                params.push(format!("{} %arg{i}", param.ll()));
                args.push(format!("{} %arg{i}", param.ll()));
            }
            let mut body = String::new();
            let mut cur = "%data".to_string();
            let mut cur_ty = base.clone();
            for (step, index) in path.iter().enumerate() {
                let Ty::Struct(struct_name) = cur_ty.under().clone() else {
                    break;
                };
                let field_ty = self.structs[&struct_name][*index].1.clone();
                let _ = writeln!(
                    body,
                    "  %f{step} = getelementptr inbounds {}, ptr {cur}, i32 0, i32 {index}",
                    Ty::Struct(struct_name).ll()
                );
                cur = format!("%f{step}");
                cur_ty = field_ty;
                if let Ty::Ptr(target) = &cur_ty {
                    let _ = writeln!(body, "  %g{step} = load ptr, ptr {cur}");
                    cur = format!("%g{step}");
                    cur_ty = target.as_ref().clone();
                }
            }
            let ret = sig.ret.ll();
            let mut operands = vec![format!("ptr {cur}")];
            operands.extend(args);
            let call = format!("call {ret} @\"{inner}\"({})", operands.join(", "));
            let tail = if sig.ret == Ty::Void {
                format!("  {call}\n  ret void")
            } else {
                format!("  %r = {call}\n  ret {ret} %r")
            };
            self.fn_defs.push(format!(
                "define internal {ret} @\"{wrapper}\"({}) {{\nentry:\n{body}{tail}\n}}\n",
                params.join(", ")
            ));
        }
        wrapper
    }

    /// The symbol to call for a method through an interface: `fn(ptr data, args...)`.
    /// A pointer-receiver method already has that shape. A value-receiver
    /// method gets a wrapper that loads the receiver from the data pointer.
    pub(super) fn iface_fn(&mut self, sig: &FuncSig) -> String {
        if sig.ptr_recv == Some(true) {
            return sig.symbol.clone();
        }
        let wrapper = format!("{}$iface", sig.symbol);
        if self.thunks.insert(wrapper.clone()) {
            let recv = sig.params[0].ll();
            let mut params = vec!["ptr %data".to_string()];
            let mut args = vec![format!("{recv} %recv")];
            for (i, ty) in sig.params.iter().enumerate().skip(1) {
                params.push(format!("{} %arg{i}", ty.ll()));
                args.push(format!("{} %arg{i}", ty.ll()));
            }
            let ret = sig.ret.ll();
            let call = format!("call {ret} @\"{}\"({})", sig.symbol, args.join(", "));
            let tail = if sig.ret == Ty::Void {
                format!("  {call}\n  ret void")
            } else {
                format!("  %r = {call}\n  ret {ret} %r")
            };
            self.fn_defs.push(format!(
                "define internal {ret} @\"{wrapper}\"({}) {{\nentry:\n  call void @goiaba.nilcheck(ptr %data)\n  %recv = load {recv}, ptr %data\n{tail}\n}}\n",
                params.join(", ")
            ));
        }
        wrapper
    }

    /// Checks that `ty` implements the interface and returns the functions
    /// that fill its method table, in the interface's method order.
    pub(super) fn implements(
        &mut self,
        ty: &Ty,
        methods: &[(String, FuncTy)],
        iface: &Ty,
    ) -> R<Vec<String>> {
        let set = self.method_set(ty);
        let mut fns = Vec::new();
        for (name, want) in methods {
            let Some((_, sig, path)) = set.iter().find(|(n, ..)| n == name) else {
                let hint = match ty.type_name() {
                    Some(t) if self.funcs.contains_key(&format!("{t}.{name}")) => {
                        format!("method {name} has pointer receiver")
                    }
                    _ => format!("missing method {name}"),
                };
                return Err(format!("{ty} does not implement {iface} ({hint})"));
            };
            if sig.params[1..] != want.params[..]
                || sig.ret != want.ret
                || sig.variadic != want.variadic
            {
                return Err(format!(
                    "{ty} does not implement {iface} (wrong type for method {name})"
                ));
            }
            fns.push(self.method_fn(ty, name, sig, path));
        }
        Ok(fns)
    }

    /// The global describing `ty` at run time: kind, size, name, equality
    /// function, method table, and enough structure (element type, fields)
    /// for the runtime to walk a value. Its address identifies the type.
    pub(super) fn rtype_of(&mut self, ty: &Ty) -> String {
        let name = ty.go_name();
        if let Some(symbol) = self.rtypes.get(&name) {
            return symbol.clone();
        }
        let symbol = format!("@\"type.{name}\"");
        // Registered first, so a recursive type can refer to itself.
        self.rtypes.insert(name.clone(), symbol.clone());

        // Kinds and flags are mirrored in runtime.c.
        let (kind, mut flags) = match ty.under() {
            Ty::Bool => (1, 0),
            Ty::Int { signed: true, .. } => (2, 0),
            Ty::Int { signed: false, .. } => (3, 0),
            Ty::Float(_) => (4, 0),
            Ty::Str => (5, 0),
            Ty::Ptr(_) => (6, 1),
            Ty::Slice(_) => (9, 0),
            Ty::Array(..) => (10, 0),
            Ty::Struct(_) => (11, 0),
            Ty::Map(..) => (12, 0),
            Ty::Chan(_) => (15, 0),
            Ty::Iface(_) => (13, 0),
            Ty::Func(_) => (14, 0),
            _ => (7, 0),
        };
        if matches!(ty.under(), Ty::Iface(methods) if !methods.is_empty()) {
            flags |= 2;
        }
        // elem: element or pointee type. count: array length or field count.
        // extra: the field table of a struct, or the key type of a map.
        let (elem, count, extra) = match ty.under().clone() {
            Ty::Ptr(inner) | Ty::Slice(inner) => (self.rtype_of(&inner), 0, "null".to_string()),
            Ty::Array(inner, n) => (self.rtype_of(&inner), n, "null".to_string()),
            Ty::Map(key, val) => {
                let key = self.rtype_of(&key);
                (self.rtype_of(&val), 0, key)
            }
            Ty::Struct(struct_name) => {
                let fields = self.structs[&struct_name].clone();
                let ll = ty.ll();
                let mut entries = Vec::new();
                for (i, (field, field_ty)) in fields.iter().enumerate() {
                    let field_type = self.rtype_of(field_ty);
                    let text = self.intern(field.as_bytes().to_vec());
                    entries.push(format!(
                        "%rfield {{ ptr {text}, i64 {}, ptr {field_type}, i64 ptrtoint (ptr getelementptr ({ll}, ptr null, i32 0, i32 {i}) to i64) }}",
                        field.len()
                    ));
                }
                if entries.is_empty() {
                    ("null".to_string(), 0, "null".to_string())
                } else {
                    let table = format!("@\"type.{name}.fields\"");
                    self.global_defs.push(format!(
                        "{table} = private constant [{} x %rfield] [{}]",
                        entries.len(),
                        entries.join(", ")
                    ));
                    ("null".to_string(), entries.len() as u64, table)
                }
            }
            _ => ("null".to_string(), 0, "null".to_string()),
        };

        let equal = self.eq_fn(ty);
        let set = self.method_set(ty);
        let mut entries = Vec::new();
        for (method, sig, path) in &set {
            let code = self.method_fn(ty, method, sig, path);
            let text = self.intern(method.as_bytes().to_vec());
            entries.push(format!(
                "%rmethod {{ ptr {text}, i64 {}, ptr @\"{code}\" }}",
                method.len()
            ));
        }
        let table = if entries.is_empty() {
            "null".to_string()
        } else {
            let table = format!("@\"type.{name}.methods\"");
            self.global_defs.push(format!(
                "{table} = private constant [{} x %rmethod] [{}]",
                entries.len(),
                entries.join(", ")
            ));
            table
        };
        let text = self.intern(name.as_bytes().to_vec());
        self.global_defs.push(format!(
            "{symbol} = private constant %rtype {{ i32 {kind}, i32 {flags}, i64 {}, ptr {text}, i64 {}, ptr {equal}, i64 {}, ptr {table}, ptr {elem}, i64 {count}, ptr {extra} }}",
            sizeof(ty),
            name.len(),
            entries.len()
        ));
        symbol
    }

    /// A function `i32 (ptr, ptr)` comparing two values of `ty` in memory,
    /// or `null` when the type is not comparable.
    pub(super) fn eq_fn(&mut self, ty: &Ty) -> String {
        let name = ty.go_name();
        if let Some(symbol) = self.eq_fns.get(&name) {
            return symbol.clone();
        }
        let symbol = format!("@\"eq.{name}\"");
        // Registered first so a recursive type refers to its own function.
        self.eq_fns.insert(name.clone(), symbol.clone());

        let saved = std::mem::replace(&mut self.f, FnCtx::new(Ty::Bool, HashSet::new()));
        let a = self.load("%a", ty);
        let b = self.load("%b", ty);
        let result = self.value_eq(ty, &a.repr, &b.repr);
        let ctx = std::mem::replace(&mut self.f, saved);
        let Ok(equal) = result else {
            self.eq_fns.insert(name, "null".to_string());
            return "null".to_string();
        };
        let mut def = format!("define internal i32 {symbol}(ptr %a, ptr %b) {{\nentry:\n");
        for line in ctx.allocas.iter().chain(&ctx.body) {
            def.push_str(line);
            def.push('\n');
        }
        let _ = write!(def, "  %r = zext i1 {equal} to i32\n  ret i32 %r\n}}\n");
        self.fn_defs.push(def);
        symbol
    }

    /// Emits a comparison of two values of `ty` and returns the i1 result.
    pub(super) fn value_eq(&mut self, ty: &Ty, a: &str, b: &str) -> R<String> {
        let ll = ty.ll();
        match ty.under() {
            Ty::Int { .. } | Ty::Bool | Ty::Ptr(_) => {
                Ok(self.ins(format!("icmp eq {ll} {a}, {b}")))
            }
            Ty::Float(_) => Ok(self.ins(format!("fcmp oeq {ll} {a}, {b}"))),
            Ty::Str => {
                let cmp = self.ins(format!("call i32 @goiaba.strcmp(%string {a}, %string {b})"));
                Ok(self.ins(format!("icmp eq i32 {cmp}, 0")))
            }
            Ty::Struct(name) => {
                let fields = self.structs[name].clone();
                let mut all = "true".to_string();
                for (i, (_, field_ty)) in fields.iter().enumerate() {
                    let fa = self.ins(format!("extractvalue {ll} {a}, {i}"));
                    let fb = self.ins(format!("extractvalue {ll} {b}, {i}"));
                    let equal = self.value_eq(field_ty, &fa, &fb)?;
                    all = self.ins(format!("and i1 {all}, {equal}"));
                }
                Ok(all)
            }
            Ty::Array(elem, n) if *n <= 16 => {
                let elem = elem.as_ref().clone();
                let mut all = "true".to_string();
                for i in 0..*n {
                    let ea = self.ins(format!("extractvalue {ll} {a}, {i}"));
                    let eb = self.ins(format!("extractvalue {ll} {b}, {i}"));
                    let equal = self.value_eq(&elem, &ea, &eb)?;
                    all = self.ins(format!("and i1 {all}, {equal}"));
                }
                Ok(all)
            }
            Ty::Iface(methods) => {
                let named = !methods.is_empty();
                let (ta, da) = self.iface_parts(a, named);
                let (tb, db) = self.iface_parts(b, named);
                let equal = self.ins(format!(
                    "call i32 @goiaba_iface_eq(ptr {ta}, ptr {da}, ptr {tb}, ptr {db})"
                ));
                Ok(self.ins(format!("icmp ne i32 {equal}, 0")))
            }
            _ => Err(format!("{ty} is not comparable")),
        }
    }

    /// Splits an interface value into its dynamic type descriptor and data
    /// pointer. `has_methods` says whether the first word is a method table.
    pub(super) fn iface_parts(&mut self, repr: &str, has_methods: bool) -> (String, String) {
        let word = self.ins(format!("extractvalue %iface {repr}, 0"));
        let data = self.ins(format!("extractvalue %iface {repr}, 1"));
        let rtype = if has_methods {
            self.ins(format!("call ptr @goiaba.itab_type(ptr {word})"))
        } else {
            word
        };
        (rtype, data)
    }

    pub(super) fn make_iface(&mut self, ty: &Ty, word: &str, data: &str) -> Val {
        let a = self.ins(format!("insertvalue %iface poison, ptr {word}, 0"));
        let b = self.ins(format!("insertvalue %iface {a}, ptr {data}, 1"));
        Val::new(ty.clone(), b)
    }

    /// The global listing an interface's method names, used to build method
    /// tables at run time.
    pub(super) fn iface_desc(&mut self, iface: &Ty, methods: &[(String, FuncTy)]) -> String {
        let name = iface.go_name();
        let symbol = format!("@\"iface.{name}\"");
        if self.iface_globals.insert(symbol.clone()) {
            let mut entries = Vec::new();
            for (method, _) in methods {
                let text = self.intern(method.as_bytes().to_vec());
                entries.push(format!(
                    "{{ ptr, i64 }} {{ ptr {text}, i64 {} }}",
                    method.len()
                ));
            }
            let list = format!("@\"iface.{name}.methods\"");
            self.global_defs.push(format!(
                "{list} = private constant [{} x {{ ptr, i64 }}] [{}]",
                entries.len(),
                entries.join(", ")
            ));
            let text = self.intern(name.as_bytes().to_vec());
            self.global_defs.push(format!(
                "{symbol} = private constant {{ i64, ptr, ptr, i64 }} {{ i64 {}, ptr {list}, ptr {text}, i64 {} }}",
                methods.len(),
                name.len()
            ));
        }
        symbol
    }

    /// Converts a value to interface type `to`.
    pub(super) fn box_iface(&mut self, val: Val, to: &Ty) -> R<Val> {
        let Ty::Iface(methods) = to.under().clone() else {
            return Err(format!("internal: {to} is not an interface"));
        };
        if let Ty::Iface(source) = val.ty.under().clone() {
            if source == methods {
                return Ok(Val {
                    ty: to.clone(),
                    ..val
                });
            }
            let covered = methods.iter().all(|m| source.contains(m));
            if !covered {
                return Err(format!(
                    "cannot use {} as {to}: need a type assertion",
                    val.ty
                ));
            }
            let (rtype, data) = self.iface_parts(&val.repr, !source.is_empty());
            if methods.is_empty() {
                return Ok(self.make_iface(to, &rtype, &data));
            }
            let desc = self.iface_desc(to, &methods);
            let tab = self.ins(format!("call ptr @goiaba_itab(ptr {desc}, ptr {rtype})"));
            return Ok(self.make_iface(to, &tab, &data));
        }

        let val = self.materialize(val)?;
        let fns = self.implements(&val.ty, &methods, to)?;
        let rtype = self.rtype_of(&val.ty);
        let word = if methods.is_empty() {
            rtype
        } else {
            let symbol = format!("@\"itab.{}.{}\"", to.go_name(), val.ty.go_name());
            if self.iface_globals.insert(symbol.clone()) {
                let mut slots = vec![format!("ptr {rtype}")];
                slots.extend(fns.iter().map(|f| format!("ptr @\"{f}\"")));
                self.global_defs.push(format!(
                    "{symbol} = private constant [{} x ptr] [{}]",
                    slots.len(),
                    slots.join(", ")
                ));
            }
            symbol
        };
        // A pointer is stored in the data word directly; other values are boxed.
        let data = if matches!(val.ty.under(), Ty::Ptr(_)) {
            val.repr.clone()
        } else {
            let slot = self.heap_alloc(&val.ty);
            self.store(&val, &slot);
            slot
        };
        Ok(self.make_iface(to, &word, &data))
    }

    /// Tests whether dynamic type `rtype` satisfies `target`. For an interface
    /// target the second result is the first word of the converted value.
    pub(super) fn assert_test(&mut self, rtype: &str, target: &Ty) -> (String, String) {
        match target.under() {
            Ty::Iface(methods) if methods.is_empty() => {
                let ok = self.ins(format!("icmp ne ptr {rtype}, null"));
                (ok, rtype.to_string())
            }
            Ty::Iface(methods) => {
                let methods = methods.clone();
                let desc = self.iface_desc(target, &methods);
                let tab = self.ins(format!("call ptr @goiaba_itab(ptr {desc}, ptr {rtype})"));
                let ok = self.ins(format!("icmp ne ptr {tab}, null"));
                (ok, tab)
            }
            _ => {
                let want = self.rtype_of(target);
                let ok = self.ins(format!("icmp eq ptr {rtype}, {want}"));
                (ok, want)
            }
        }
    }

    /// Reads the value of type `target` out of an interface whose test passed.
    pub(super) fn assert_value(&mut self, word: &str, data: &str, target: &Ty) -> Val {
        match target.under() {
            Ty::Iface(_) => self.make_iface(target, word, data),
            Ty::Ptr(_) => Val::new(target.clone(), data),
            _ => self.load(data, target),
        }
    }

    /// `x.(T)`. With `comma_ok` the result is the tuple `(T, bool)`.
    pub(super) fn type_assert(&mut self, ta: &ast::TypeAssertExpr, comma_ok: bool) -> R<Val> {
        let Some(typ) = &ta.typ else {
            return Err("x.(type) is only allowed in a type switch".into());
        };
        let subject = self.expr_m(&ta.expr)?;
        let Ty::Iface(source) = subject.ty.under().clone() else {
            return Err(format!("{} is not an interface", subject.ty));
        };
        let target = self.resolve_type(typ)?;
        let (rtype, data) = self.iface_parts(&subject.repr, !source.is_empty());
        let (ok, word) = self.assert_test(&rtype, &target);

        if !comma_ok {
            let iface_name = subject.ty.go_name();
            let want_name = target.go_name();
            let iface_text = self.intern(iface_name.as_bytes().to_vec());
            let want_text = self.intern(want_name.as_bytes().to_vec());
            let want_iface = matches!(target.under(), Ty::Iface(_)) as i32;
            self.emit(format!(
                "call void @goiaba.assert_ok(i1 {ok}, ptr {rtype}, ptr {iface_text}, i64 {}, ptr {want_text}, i64 {}, i32 {want_iface})",
                iface_name.len(),
                want_name.len()
            ));
            return Ok(self.assert_value(&word, &data, &target));
        }

        // On failure the value is T's zero value, so only read it when ok.
        let yes = self.new_label("assert.ok");
        let end = self.new_label("assert.end");
        let from = self.f.cur.clone();
        self.cond_br(&ok, &yes, &end);
        self.place_label(&yes);
        let val = self.assert_value(&word, &data, &target);
        let yes_end = self.f.cur.clone();
        self.br(&end);
        self.place_label(&end);
        let ll = target.ll();
        let merged = self.ins(format!(
            "phi {ll} [ zeroinitializer, %{from} ], [ {}, %{yes_end} ]",
            val.repr
        ));
        let tuple = Ty::Tuple(vec![target.clone(), Ty::Bool]);
        let tuple_ll = tuple.ll();
        let a = self.ins(format!("insertvalue {tuple_ll} poison, {ll} {merged}, 0"));
        let b = self.ins(format!("insertvalue {tuple_ll} {a}, i1 {ok}, 1"));
        Ok(Val::new(tuple, b))
    }

    pub(super) fn type_switch(&mut self, stmt: &ast::TypeSwitchStmt) -> R<()> {
        let objs = self.objs;
        self.push_scope();
        let result = (|| {
            if let Some(init) = &stmt.init {
                self.stmt(init)?;
            }
            // `switch x.(type)` or `switch v := x.(type)`.
            let (bind, guard) = match &stmt.assign {
                Stmt::Expr(e) => (None, e.as_ref()),
                Stmt::Assign(key) => {
                    let a = &objs.a_stmts[*key];
                    let (Some(Expr::Ident(name)), Some(rhs)) = (a.lhs.first(), a.rhs.first())
                    else {
                        return Err("invalid type switch guard".to_string());
                    };
                    (Some(objs.idents[*name].name.as_str()), rhs)
                }
                _ => return Err("invalid type switch guard".to_string()),
            };
            let Expr::TypeAssert(ta) = strip_parens(guard) else {
                return Err("invalid type switch guard".to_string());
            };
            let subject = self.expr_m(&ta.expr)?;
            let Ty::Iface(source) = subject.ty.under().clone() else {
                return Err(format!("{} is not an interface", subject.ty));
            };
            let (rtype, data) = self.iface_parts(&subject.repr, !source.is_empty());

            let end = self.new_label("typeswitch.end");
            let mut clauses = Vec::new();
            for s in &stmt.body.list {
                let Stmt::Case(clause) = s else {
                    return Err("unexpected statement in type switch body".to_string());
                };
                clauses.push((clause, self.new_label("typeswitch.case")));
            }

            // For each clause: the single type it binds to, if it has exactly one.
            let mut bound = Vec::new();
            let mut default = None;
            for (clause, label) in &clauses {
                let Some(types) = clause.list.as_ref().filter(|l| !l.is_empty()) else {
                    default = Some(label.clone());
                    bound.push(None);
                    continue;
                };
                let mut single = None;
                for type_expr in types {
                    let is_nil = matches!(strip_parens(type_expr), Expr::Ident(k) if objs.idents[*k].name == "nil");
                    let hit = if is_nil {
                        self.ins(format!("icmp eq ptr {rtype}, null"))
                    } else {
                        let target = self.resolve_type(type_expr)?;
                        let (ok, _) = self.assert_test(&rtype, &target);
                        single = Some(target);
                        ok
                    };
                    let next = self.new_label("typeswitch.next");
                    self.cond_br(&hit, label, &next);
                    self.place_label(&next);
                }
                bound.push(if types.len() == 1 { single } else { None });
            }
            self.br(default.as_deref().unwrap_or(&end));

            for ((clause, label), target) in clauses.iter().zip(bound) {
                self.place_label(label);
                self.push_scope();
                if let Some(name) = bind {
                    let val = match &target {
                        Some(target) => {
                            let (_, word) = self.assert_test(&rtype, target);
                            self.assert_value(&word, &data, target)
                        }
                        None => subject.clone(),
                    };
                    self.declare(name, val.ty.clone(), Some(val));
                }
                self.push_target(end.clone(), None);
                let r = self.block(&clause.body);
                self.f.targets.pop();
                self.pop_scope();
                r?;
                self.br(&end);
            }
            self.place_label(&end);
            Ok(())
        })();
        self.pop_scope();
        result
    }
}
