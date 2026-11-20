// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! Expressions: places and values, operators, literals, indexing and maps.

use super::*;

/// The left side of an assignment, with its operands already evaluated.
pub(super) enum Target {
    /// An addressable location.
    Place(String, Ty),
    /// A map element: the map, a slot holding the key, and the value type.
    MapSlot { map: String, key: String, val: Ty },
    /// The blank identifier.
    Blank,
}

impl Target {
    /// The type a value must have to be stored; `None` for the blank identifier.
    pub(super) fn ty(&self) -> Option<Ty> {
        match self {
            Target::Place(_, ty) => Some(ty.clone()),
            Target::MapSlot { val, .. } => Some(val.clone()),
            Target::Blank => None,
        }
    }
}

impl<'a> Compiler<'a> {
    pub(super) fn map_new(&mut self, key: &Ty, val: &Ty, hint: &str) -> String {
        let key_type = self.rtype_of(key);
        self.ins(format!(
            "call ptr @goiaba_map_new(ptr {key_type}, i64 {}, i64 {hint})",
            sizeof(val)
        ))
    }

    /// `m[k]` as a value: the stored value, or the zero value when the key is
    /// absent. With `comma_ok` the result is the tuple `(V, bool)`.
    pub(super) fn map_get(
        &mut self,
        map: &str,
        key: &Ty,
        val: &Ty,
        index: &Expr,
        comma_ok: bool,
    ) -> R<Val> {
        let k = self.expr_want(index, key)?;
        let key_slot = self.spill(&k);
        let (merged, ok) = self.map_lookup(map, &key_slot, val);
        if !comma_ok {
            return Ok(merged);
        }
        let ll = val.ll();
        let tuple = Ty::Tuple(vec![val.clone(), Ty::Bool]);
        let tuple_ll = tuple.ll();
        let a = self.ins(format!(
            "insertvalue {tuple_ll} poison, {ll} {}, 0",
            merged.repr
        ));
        let b = self.ins(format!("insertvalue {tuple_ll} {a}, i1 {ok}, 1"));
        Ok(Val::new(tuple, b))
    }

    /// Reads the value stored under the key in `key_slot` without inserting:
    /// returns the value (zero when absent) and the i1 "present" flag.
    fn map_lookup(&mut self, map: &str, key_slot: &str, val: &Ty) -> (Val, String) {
        let found = self.ins(format!(
            "call ptr @goiaba_map_get(ptr {map}, ptr {key_slot})"
        ));
        let ok = self.ins(format!("icmp ne ptr {found}, null"));
        let yes = self.new_label("map.hit");
        let end = self.new_label("map.end");
        let from = self.f.cur.clone();
        self.cond_br(&ok, &yes, &end);
        self.place_label(&yes);
        let stored = self.load(&found, val);
        self.br(&end);
        self.place_label(&end);
        let merged = self.ins(format!(
            "phi {} [ zeroinitializer, %{from} ], [ {}, %{yes} ]",
            val.ll(),
            stored.repr
        ));
        (Val::new(val.clone(), merged), ok)
    }

    /// Evaluates the operands of an assignment target without writing to it.
    /// A map element is not inserted yet: Go evaluates the right side first,
    /// and that may itself change the map.
    pub(super) fn target(&mut self, e: &Expr) -> R<Target> {
        let e = strip_parens(e);
        if let Expr::Ident(key) = e
            && self.objs.idents[*key].name == "_"
        {
            return Ok(Target::Blank);
        }
        if let Expr::Index(ix) = e {
            let base = self.loc(&ix.expr)?;
            let base_ty = match &base {
                Loc::Place(_, ty) => ty.clone(),
                Loc::Value(val) => val.ty.clone(),
            };
            if let Ty::Map(key, val) = base_ty.under() {
                let map = match base {
                    Loc::Place(ptr, ty) => self.load(&ptr, &ty),
                    Loc::Value(val) => val,
                };
                let k = self.expr_want(&ix.index, key)?;
                return Ok(Target::MapSlot {
                    map: map.repr,
                    key: self.spill(&k),
                    val: val.as_ref().clone(),
                });
            }
        }
        match self.loc(e)? {
            Loc::Place(ptr, ty) => Ok(Target::Place(ptr, ty)),
            Loc::Value(_) => Err("cannot assign to this expression".into()),
        }
    }

    /// The current value of a target, for `x += y` and `x++`. An absent map
    /// key reads as the zero value.
    pub(super) fn target_load(&mut self, target: &Target) -> Val {
        match target {
            Target::Place(ptr, ty) => self.load(ptr, ty),
            Target::MapSlot { map, key, val } => self.map_lookup(map, key, val).0,
            Target::Blank => Val::void(),
        }
    }

    pub(super) fn target_store(&mut self, target: &Target, val: &Val) {
        match target {
            Target::Place(ptr, _) => self.store(val, ptr),
            Target::MapSlot { map, key, .. } => {
                let slot = self.ins(format!("call ptr @goiaba_map_assign(ptr {map}, ptr {key})"));
                self.store(val, &slot);
            }
            Target::Blank => {}
        }
    }

    /// Evaluates an expression. The result may be an untyped constant.
    pub(super) fn expr(&mut self, e: &Expr) -> R<Val> {
        match self.loc(e)? {
            Loc::Place(ptr, ty) => Ok(self.load(&ptr, &ty)),
            Loc::Value(val) => Ok(val),
        }
    }

    /// Evaluates an expression and gives untyped constants their default type.
    pub(super) fn expr_m(&mut self, e: &Expr) -> R<Val> {
        let val = self.expr(e)?;
        self.materialize(val)
    }

    /// Evaluates an expression that must be assignable to `want`.
    pub(super) fn expr_want(&mut self, e: &Expr, want: &Ty) -> R<Val> {
        let hint = matches!(want.under(), Ty::Int { .. }).then(|| want.clone());
        let saved = std::mem::replace(&mut self.shift_hint, hint);
        let val = match strip_parens(e) {
            Expr::CompositeLit(lit) if lit.typ.is_none() => self.composite(lit, Some(want)),
            _ => self.expr(e),
        };
        self.shift_hint = saved;
        self.coerce(val?, want)
    }

    pub(super) fn loc(&mut self, e: &Expr) -> R<Loc> {
        let result = self.loc_inner(e);
        result.map_err(|err| self.locate(e.pos(self.objs), err))
    }

    pub(super) fn loc_inner(&mut self, e: &Expr) -> R<Loc> {
        let objs = self.objs;
        match e {
            Expr::Paren(p) => self.loc(&p.expr),
            Expr::Ident(key) => {
                let name = objs.idents[*key].name.as_str();
                match self.lookup(name) {
                    Some(Binding::Var {
                        ptr, ty, indirect, ..
                    }) => {
                        let ptr = if indirect {
                            self.ins(format!("load ptr, ptr {ptr}"))
                        } else {
                            ptr
                        };
                        Ok(Loc::Place(ptr, ty))
                    }
                    Some(Binding::Const(val)) => Ok(Loc::Value(val)),
                    Some(Binding::Type(ty)) => Err(format!("{ty} is a type, not a value")),
                    None => match name {
                        "true" => Ok(Loc::Value(Val::bool(true))),
                        "false" => Ok(Loc::Value(Val::bool(false))),
                        "nil" => Ok(Loc::Value(Val::new(Ty::Nil, "null"))),
                        "_" => Err("cannot use _ as a value".into()),
                        _ if self.funcs.contains_key(&self.q(name)) => {
                            let key = self.q(name);
                            Ok(Loc::Value(self.func_value(&key)?))
                        }
                        _ => Err(format!("undefined: {name}")),
                    },
                }
            }
            Expr::Star(s) => self.deref(&s.expr),
            Expr::Unary(u) if matches!(u.op, Token::MUL) => self.deref(&u.expr),
            Expr::Selector(s) => self.field(s),
            Expr::Index(ix) => self.index(ix),
            Expr::IndexList(ix) => match self.instance_value(&ix.expr, &ix.indices)? {
                Some(val) => Ok(Loc::Value(val)),
                None => Err("several indexes are only valid as type arguments".into()),
            },
            _ => Ok(Loc::Value(self.rvalue(e)?)),
        }
    }

    pub(super) fn deref(&mut self, e: &Expr) -> R<Loc> {
        let val = self.expr(e)?;
        match val.ty.under() {
            Ty::Ptr(inner) => {
                self.nilcheck(&val.repr);
                Ok(Loc::Place(val.repr, inner.as_ref().clone()))
            }
            _ => Err(format!("cannot dereference {}", val.ty)),
        }
    }

    /// Returns a pointer to the struct at `loc`, following one pointer level.
    pub(super) fn struct_ptr(&mut self, loc: Loc) -> R<(String, String)> {
        let val = match loc {
            Loc::Place(ptr, ty) => match ty.under() {
                Ty::Struct(name) => return Ok((ptr, name.clone())),
                _ => self.load(&ptr, &ty),
            },
            Loc::Value(val) => val,
        };
        match val.ty.under() {
            Ty::Ptr(inner) => match inner.under() {
                Ty::Struct(name) => {
                    self.nilcheck(&val.repr);
                    Ok((val.repr.clone(), name.clone()))
                }
                _ => Err(format!("{} has no fields", val.ty)),
            },
            Ty::Struct(name) => Ok((self.spill(&val), name.clone())),
            _ => Err(format!("{} has no fields", val.ty)),
        }
    }

    pub(super) fn field(&mut self, s: &ast::SelectorExpr) -> R<Loc> {
        let sel = self.objs.idents[s.sel].name.as_str();
        if let Some(package) = self.package_of(&s.expr) {
            // `pkg.Name`: a variable, constant or function of another package.
            let key = format!("{package}.{sel}");
            return match self.globals.get(&key).cloned() {
                Some(Binding::Var { ptr, ty, .. }) => Ok(Loc::Place(ptr, ty)),
                Some(Binding::Const(val)) => Ok(Loc::Value(val)),
                Some(Binding::Type(ty)) => Err(format!("{ty} is a type, not a value")),
                None if self.funcs.contains_key(&key) => Ok(Loc::Value(self.func_value(&key)?)),
                None => Err(format!("undefined: {key}")),
            };
        }
        let base = self.loc(&s.expr)?;
        let base = self.promoted_receiver(base, sel)?;
        if let Some(value) = self.method_value(&base, sel, &s.expr)? {
            return Ok(Loc::Value(value));
        }
        self.field_of(base, sel)
    }

    /// `x.Method` used as a value: a function value bound to the receiver.
    /// Returns `None` when the type has no such method.
    pub(super) fn method_value(
        &mut self,
        base: &Loc,
        sel: &str,
        recv_expr: &Expr,
    ) -> R<Option<Val>> {
        let recv_ty = match base {
            Loc::Place(_, ty) => ty.clone(),
            Loc::Value(val) => val.ty.clone(),
        };
        let load = |c: &mut Self, base: &Loc| match base {
            Loc::Place(ptr, ty) => c.load(ptr, ty),
            Loc::Value(val) => val.clone(),
        };
        let bind = |c: &mut Self, sig: FuncTy, code: &str, ctx: &str| {
            let a = c.ins(format!("insertvalue %func poison, ptr {code}, 0"));
            let b = c.ins(format!("insertvalue %func {a}, ptr {ctx}, 1"));
            Val::new(Ty::Func(Box::new(sig)), b)
        };

        // On an interface: the table entry already takes the data word first,
        // which is exactly how a function value passes its context.
        if let Ty::Iface(methods) = recv_ty.under() {
            let Some(index) = methods.iter().position(|(name, _)| name == sel) else {
                return Ok(None);
            };
            let sig = methods[index].1.clone();
            let val = load(self, base);
            let tab = self.ins(format!("extractvalue %iface {}, 0", val.repr));
            let data = self.ins(format!("extractvalue %iface {}, 1", val.repr));
            self.nilcheck(&tab);
            let slot = self.ins(format!(
                "getelementptr inbounds ptr, ptr {tab}, i64 {}",
                index + 1
            ));
            let code = self.ins(format!("load ptr, ptr {slot}"));
            return Ok(Some(bind(self, sig, &code, &data)));
        }

        let (base_ty, recv_is_ptr) = match &recv_ty {
            Ty::Ptr(inner) => (inner.as_ref().clone(), true),
            other => (other.clone(), false),
        };
        let Some(type_name) = base_ty.type_name() else {
            return Ok(None);
        };
        let Some(sig) = self.funcs.get(&format!("{type_name}.{sel}")).cloned() else {
            return Ok(None);
        };
        let code = format!("@\"{}\"", self.iface_fn(&sig));
        // The context is a pointer to the receiver. A value receiver is
        // copied now, as Go evaluates it when the method value is made.
        let ctx = if recv_is_ptr {
            load(self, base).repr
        } else if sig.ptr_recv == Some(true) {
            match base {
                Loc::Place(ptr, _) => {
                    self.mark_escape(recv_expr);
                    ptr.clone()
                }
                Loc::Value(val) => self.spill(val),
            }
        } else {
            let val = load(self, base);
            let copy = self.heap_alloc(&val.ty);
            self.store(&val, &copy);
            copy
        };
        let func_ty = FuncTy {
            params: sig.params[1..].to_vec(),
            ret: sig.ret.clone(),
            variadic: sig.variadic,
        };
        Ok(Some(bind(self, func_ty, &code, &ctx)))
    }

    pub(super) fn field_of(&mut self, base: Loc, sel: &str) -> R<Loc> {
        let (ptr, struct_name) = self.struct_ptr(base)?;
        let Some(path) = self.field_path(&struct_name, sel) else {
            return Err(format!("{struct_name} has no field or method {sel}"));
        };
        self.walk_fields(Loc::Place(ptr, Ty::Struct(struct_name)), &path)
    }

    /// The package `e` names, when `e` is an imported package's identifier.
    pub(super) fn package_of(&mut self, e: &Expr) -> Option<String> {
        let Expr::Ident(key) = strip_parens(e) else {
            return None;
        };
        let name = self.objs.idents[*key].name.as_str();
        if self.lookup(name).is_some() {
            return None;
        }
        self.packages.get(name).cloned()
    }

    pub(super) fn index(&mut self, ix: &ast::IndexExpr) -> R<Loc> {
        if let Some(val) = self.instance_value(&ix.expr, std::slice::from_ref(&ix.index))? {
            return Ok(Loc::Value(val));
        }
        let base = match self.loc(&ix.expr)? {
            Loc::Place(ptr, ty) if matches!(ty.under(), Ty::Array(..)) => {
                Loc::Place(ptr, ty.under().clone())
            }
            other => other,
        };
        let base_ty = match &base {
            Loc::Place(_, ty) => ty.clone(),
            Loc::Value(val) => val.ty.clone(),
        };
        if let Ty::Map(key, val) = base_ty.under() {
            let map = match base {
                Loc::Place(ptr, ty) => self.load(&ptr, &ty),
                Loc::Value(val) => val,
            };
            return Ok(Loc::Value(
                self.map_get(&map.repr, key, val, &ix.index, false)?,
            ));
        }
        let idx_val = self.expr(&ix.index)?;
        let idx = self.widen_i64(idx_val)?;

        let array_at = |c: &mut Self, ptr: &str, elem: &Ty, n: u64| {
            c.emit(format!("call void @goiaba.bounds(i64 {idx}, i64 {n})"));
            let place = c.ins(format!(
                "getelementptr inbounds [{n} x {}], ptr {ptr}, i64 0, i64 {idx}",
                elem.ll()
            ));
            Loc::Place(place, elem.clone())
        };

        let val = match base {
            Loc::Place(ptr, Ty::Array(elem, n)) => return Ok(array_at(self, &ptr, &elem, n)),
            Loc::Place(ptr, ty) => self.load(&ptr, &ty),
            Loc::Value(val) => val,
        };
        match val.ty.under() {
            Ty::Slice(elem) => {
                let data = self.ins(format!("extractvalue %slice {}, 0", val.repr));
                let len = self.ins(format!("extractvalue %slice {}, 1", val.repr));
                self.emit(format!("call void @goiaba.bounds(i64 {idx}, i64 {len})"));
                let place = self.ins(format!(
                    "getelementptr inbounds {}, ptr {data}, i64 {idx}",
                    elem.ll()
                ));
                Ok(Loc::Place(place, elem.as_ref().clone()))
            }
            Ty::Ptr(inner) => match inner.as_ref() {
                Ty::Array(elem, n) => {
                    self.nilcheck(&val.repr);
                    Ok(array_at(self, &val.repr, elem, *n))
                }
                other => Err(format!("cannot index *{other}")),
            },
            Ty::Array(elem, n) => {
                let slot = self.spill(&val);
                Ok(array_at(self, &slot, elem, *n))
            }
            Ty::Str => {
                let data = self.ins(format!("extractvalue %string {}, 0", val.repr));
                let len = self.ins(format!("extractvalue %string {}, 1", val.repr));
                self.emit(format!("call void @goiaba.bounds(i64 {idx}, i64 {len})"));
                let place = self.ins(format!("getelementptr inbounds i8, ptr {data}, i64 {idx}"));
                let byte = Ty::Int {
                    bits: 8,
                    signed: false,
                };
                Ok(Loc::Value(self.load(&place, &byte)))
            }
            other => Err(format!("cannot index {other}")),
        }
    }

    pub(super) fn rvalue(&mut self, e: &Expr) -> R<Val> {
        match e {
            Expr::BasicLit(lit) => self.literal(&lit.token),
            Expr::Binary(b) => match b.op {
                Token::LAND => self.logical(b, true),
                Token::LOR => self.logical(b, false),
                _ => {
                    let op = Op::from_binary(&b.op)
                        .ok_or_else(|| format!("unsupported binary operator {:?}", b.op))?;
                    let left = self.expr(&b.expr_a)?;
                    // A typed left operand also types an untyped shift on the right.
                    let typed = matches!(left.ty.under(), Ty::Int { .. })
                        && !matches!(op, Op::Shl | Op::Shr);
                    let saved = self.shift_hint.clone();
                    if typed {
                        self.shift_hint = Some(left.ty.clone());
                    }
                    let right = self.expr(&b.expr_b);
                    self.shift_hint = saved;
                    self.binary(op, left, right?)
                }
            },
            Expr::Unary(u) => self.unary(u),
            Expr::Call(call) => self.call(call),
            Expr::CompositeLit(lit) => self.composite(lit, None),
            Expr::Slice(s) => self.slice_expr(s),
            Expr::FuncLit(lit) => self.func_lit(lit),
            Expr::TypeAssert(ta) => self.type_assert(ta, false),
            Expr::KeyValue(_) => Err("unexpected key: value pair".into()),
            _ => Err("unsupported expression".into()),
        }
    }

    pub(super) fn literal(&mut self, token: &Token) -> R<Val> {
        let text = token.get_literal();
        match token {
            Token::INT(_) => Ok(Val::int(Ty::UntypedInt, parse_int(text)?)),
            Token::CHAR(_) => Ok(Val::int(Ty::UntypedRune, unquote_rune(text)?)),
            Token::FLOAT(_) => {
                let v: f64 = text
                    .replace('_', "")
                    .parse()
                    .map_err(|_| format!("invalid float literal {text}"))?;
                Ok(Val::float(Ty::UntypedFloat, v))
            }
            Token::STRING(_) => {
                let bytes = unquote_string(text)?;
                Ok(self.string_const(bytes))
            }
            _ => Err(format!("unsupported literal {text}")),
        }
    }

    /// `&&` and `||`: the right side runs only when the left does not decide.
    pub(super) fn logical(&mut self, b: &ast::BinaryExpr, is_and: bool) -> R<Val> {
        let left = self.expr_want(&b.expr_a, &Ty::Bool)?;
        let left_block = self.f.cur.clone();
        let rhs = self.new_label("logic.rhs");
        let end = self.new_label("logic.end");
        if is_and {
            self.cond_br(&left.repr, &rhs, &end);
        } else {
            self.cond_br(&left.repr, &end, &rhs);
        }
        self.place_label(&rhs);
        let right = self.expr_want(&b.expr_b, &Ty::Bool)?;
        let right_block = self.f.cur.clone();
        self.br(&end);
        self.place_label(&end);
        let short = if is_and { "false" } else { "true" };
        let name = self.ins(format!(
            "phi i1 [ {short}, %{left_block} ], [ {}, %{right_block} ]",
            right.repr
        ));
        Ok(Val::new(Ty::Bool, name))
    }

    pub(super) fn unary(&mut self, u: &ast::UnaryExpr) -> R<Val> {
        if matches!(u.op, Token::AND) {
            if let Expr::CompositeLit(lit) = strip_parens(&u.expr) {
                let val = self.composite(lit, None)?;
                let ptr = self.heap_alloc(&val.ty);
                self.store(&val, &ptr);
                return Ok(Val::new(Ty::Ptr(Box::new(val.ty)), ptr));
            }
            self.mark_escape(&u.expr);
            return match self.loc(&u.expr)? {
                Loc::Place(ptr, ty) => Ok(Val::new(Ty::Ptr(Box::new(ty)), ptr)),
                Loc::Value(_) => Err("cannot take the address of this expression".into()),
            };
        }

        if matches!(u.op, Token::ARROW) {
            let (val, _) = self.chan_recv(&u.expr)?;
            return Ok(val);
        }

        let val = self.expr(&u.expr)?;
        match (&u.op, val.ty.under(), val.k) {
            (Token::ADD, ty, _)
                if ty.is_untyped_num() || matches!(ty, Ty::Int { .. } | Ty::Float(_)) =>
            {
                Ok(val)
            }
            (Token::SUB, ty, Some(K::Int(v))) if ty.is_untyped_num() => {
                Ok(Val::int(val.ty.clone(), -v))
            }
            (Token::SUB, ty, Some(K::Float(v))) if ty.is_untyped_num() => {
                Ok(Val::float(val.ty.clone(), -v))
            }
            (Token::XOR, ty, Some(K::Int(v))) if ty.is_untyped_num() => {
                Ok(Val::int(val.ty.clone(), !v))
            }
            (Token::SUB, Ty::Int { .. }, Some(K::Int(v))) => {
                let ty = val.ty.clone();
                self.convert(Val::int(Ty::UntypedInt, -v), &ty)
            }
            (Token::SUB, Ty::Float(_), Some(K::Float(v))) => {
                let ty = val.ty.clone();
                self.convert(Val::float(Ty::UntypedFloat, -v), &ty)
            }
            (Token::SUB, Ty::Int { .. }, _) => {
                let name = self.ins(format!("sub {} 0, {}", val.ty.ll(), val.repr));
                Ok(Val::new(val.ty, name))
            }
            (Token::SUB, Ty::Float(_), _) => {
                let name = self.ins(format!("fneg {} {}", val.ty.ll(), val.repr));
                Ok(Val::new(val.ty, name))
            }
            (Token::XOR, Ty::Int { .. }, _) => {
                let name = self.ins(format!("xor {} {}, -1", val.ty.ll(), val.repr));
                Ok(Val::new(val.ty, name))
            }
            (Token::NOT, Ty::Bool, _) => {
                let name = self.ins(format!("xor i1 {}, true", val.repr));
                Ok(Val::new(Ty::Bool, name))
            }
            (op, ty, _) => Err(format!("unsupported unary operator {op:?} on {ty}")),
        }
    }

    pub(super) fn binary(&mut self, op: Op, left: Val, right: Val) -> R<Val> {
        if matches!(op, Op::Shl | Op::Shr) {
            return self.shift(op, left, right);
        }
        if left.ty.is_untyped_num() && right.ty.is_untyped_num() {
            return fold(op, &left, &right);
        }

        if left.ty == Ty::Nil || right.ty == Ty::Nil {
            let other = if left.ty == Ty::Nil { &right } else { &left };
            let ptr = match other.ty.under() {
                Ty::Ptr(_) | Ty::Map(..) | Ty::Chan(_) => other.repr.clone(),
                Ty::Slice(_) => self.ins(format!("extractvalue %slice {}, 0", other.repr)),
                Ty::Func(_) => self.ins(format!("extractvalue %func {}, 0", other.repr)),
                Ty::Iface(_) => self.ins(format!("extractvalue %iface {}, 0", other.repr)),
                ty => return Err(format!("cannot compare {ty} with nil")),
            };
            let pred = match op {
                Op::Eq => "eq",
                Op::Ne => "ne",
                _ => return Err(format!("operator {} is not defined on nil", op.symbol())),
            };
            let name = self.ins(format!("icmp {pred} ptr {ptr}, null"));
            return Ok(Val::new(Ty::Bool, name));
        }

        // Comparing with an interface compares dynamic types, then values.
        let left_iface = matches!(left.ty.under(), Ty::Iface(_));
        let right_iface = matches!(right.ty.under(), Ty::Iface(_));
        if left_iface || right_iface {
            let (left, right) = if !left_iface {
                let ty = right.ty.clone();
                (self.coerce(left, &ty)?, right)
            } else if !right_iface {
                let ty = left.ty.clone();
                let right = self.coerce(right, &ty)?;
                (left, right)
            } else {
                (left, right)
            };
            let negate = match op {
                Op::Eq => false,
                Op::Ne => true,
                _ => {
                    return Err(format!(
                        "operator {} is not defined on interfaces",
                        op.symbol()
                    ));
                }
            };
            let ty = left.ty.clone();
            let equal = self.value_eq(&ty, &left.repr, &right.repr)?;
            let name = if negate {
                self.ins(format!("xor i1 {equal}, true"))
            } else {
                equal
            };
            return Ok(Val::new(Ty::Bool, name));
        }

        let (left, right) = if left.ty.is_untyped_num() {
            let ty = right.ty.clone();
            (self.coerce(left, &ty)?, right)
        } else if right.ty.is_untyped_num() {
            let ty = left.ty.clone();
            let right = self.coerce(right, &ty)?;
            (left, right)
        } else {
            (left, right)
        };
        if left.ty != right.ty {
            return Err(format!(
                "mismatched types {} and {} for operator {}",
                left.ty,
                right.ty,
                op.symbol()
            ));
        }

        let ty = left.ty.clone();
        // Two typed constants fold to a constant of the same type, so that
        // `const Second = 1000 * Millisecond` stays a constant.
        if let (Some(lk), Some(rk)) = (left.k, right.k)
            && matches!(ty.under(), Ty::Int { .. } | Ty::Float(_))
        {
            let untyped = if matches!(ty.under(), Ty::Float(_)) {
                Ty::UntypedFloat
            } else {
                Ty::UntypedInt
            };
            let as_untyped = |k: K| match k {
                K::Int(v) if untyped == Ty::UntypedInt => Val::int(Ty::UntypedInt, v),
                k => Val::float(Ty::UntypedFloat, k.as_f64()),
            };
            let folded = fold(op, &as_untyped(lk), &as_untyped(rk))?;
            return if op.is_comparison() {
                Ok(folded)
            } else {
                self.convert(folded, &ty)
            };
        }
        let ll = ty.ll();
        let (a, b) = (left.repr.as_str(), right.repr.as_str());
        let undefined = || format!("operator {} is not defined on {ty}", op.symbol());

        match ty.under() {
            Ty::Int { signed, .. } => {
                if op.is_comparison() {
                    let pred = match (op, signed) {
                        (Op::Eq, _) => "eq",
                        (Op::Ne, _) => "ne",
                        (Op::Lt, true) => "slt",
                        (Op::Le, true) => "sle",
                        (Op::Gt, true) => "sgt",
                        (Op::Ge, true) => "sge",
                        (Op::Lt, false) => "ult",
                        (Op::Le, false) => "ule",
                        (Op::Gt, false) => "ugt",
                        _ => "uge",
                    };
                    let name = self.ins(format!("icmp {pred} {ll} {a}, {b}"));
                    return Ok(Val::new(Ty::Bool, name));
                }
                let name = match op {
                    Op::Add => self.ins(format!("add {ll} {a}, {b}")),
                    Op::Sub => self.ins(format!("sub {ll} {a}, {b}")),
                    Op::Mul => self.ins(format!("mul {ll} {a}, {b}")),
                    Op::And => self.ins(format!("and {ll} {a}, {b}")),
                    Op::Or => self.ins(format!("or {ll} {a}, {b}")),
                    Op::Xor => self.ins(format!("xor {ll} {a}, {b}")),
                    Op::AndNot => {
                        let inv = self.ins(format!("xor {ll} {b}, -1"));
                        self.ins(format!("and {ll} {a}, {inv}"))
                    }
                    Op::Div | Op::Rem => self.int_div(op, &left, &right, *signed)?,
                    _ => return Err(undefined()),
                };
                Ok(Val::new(ty, name))
            }
            Ty::Float(_) => {
                if op.is_comparison() {
                    let pred = match op {
                        Op::Eq => "oeq",
                        Op::Ne => "une",
                        Op::Lt => "olt",
                        Op::Le => "ole",
                        Op::Gt => "ogt",
                        _ => "oge",
                    };
                    let name = self.ins(format!("fcmp {pred} {ll} {a}, {b}"));
                    return Ok(Val::new(Ty::Bool, name));
                }
                let inst = match op {
                    Op::Add => "fadd",
                    Op::Sub => "fsub",
                    Op::Mul => "fmul",
                    Op::Div => "fdiv",
                    _ => return Err(undefined()),
                };
                let name = self.ins(format!("{inst} {ll} {a}, {b}"));
                Ok(Val::new(ty, name))
            }
            Ty::Bool | Ty::Ptr(_) => {
                let pred = match op {
                    Op::Eq => "eq",
                    Op::Ne => "ne",
                    _ => return Err(undefined()),
                };
                let name = self.ins(format!("icmp {pred} {ll} {a}, {b}"));
                Ok(Val::new(Ty::Bool, name))
            }
            Ty::Str => {
                if op == Op::Add {
                    let name = self.ins(format!(
                        "call %string @goiaba.concat(%string {a}, %string {b})"
                    ));
                    return Ok(Val::new(Ty::Str, name));
                }
                let pred = match op {
                    Op::Eq => "eq",
                    Op::Ne => "ne",
                    Op::Lt => "slt",
                    Op::Le => "sle",
                    Op::Gt => "sgt",
                    Op::Ge => "sge",
                    _ => return Err(undefined()),
                };
                let cmp = self.ins(format!("call i32 @goiaba.strcmp(%string {a}, %string {b})"));
                let name = self.ins(format!("icmp {pred} i32 {cmp}, 0"));
                Ok(Val::new(Ty::Bool, name))
            }
            Ty::Struct(_) | Ty::Array(..) if matches!(op, Op::Eq | Op::Ne) => {
                let equal = self.value_eq(&ty, a, b)?;
                let name = if op == Op::Ne {
                    self.ins(format!("xor i1 {equal}, true"))
                } else {
                    equal
                };
                Ok(Val::new(Ty::Bool, name))
            }
            _ => Err(undefined()),
        }
    }

    /// Integer `/` and `%` with Go's semantics: a zero divisor panics and
    /// `MinInt / -1` wraps. Both are undefined behaviour for LLVM's sdiv/srem.
    pub(super) fn int_div(&mut self, op: Op, left: &Val, right: &Val, signed: bool) -> R<String> {
        let ll = left.ty.ll();
        let (a, b) = (left.repr.as_str(), right.repr.as_str());
        let inst = match (op, signed) {
            (Op::Div, true) => "sdiv",
            (Op::Div, false) => "udiv",
            (_, true) => "srem",
            (_, false) => "urem",
        };
        match right.k {
            Some(K::Int(0)) => return Err("division by zero".into()),
            Some(K::Int(-1)) if signed => {
                return Ok(if op == Op::Div {
                    self.ins(format!("sub {ll} 0, {a}"))
                } else {
                    "0".to_string()
                });
            }
            Some(_) => return Ok(self.ins(format!("{inst} {ll} {a}, {b}"))),
            None => {}
        }
        let is_zero = self.ins(format!("icmp eq {ll} {b}, 0"));
        self.panic_if(&is_zero, "integer divide by zero");
        if !signed {
            return Ok(self.ins(format!("{inst} {ll} {a}, {b}")));
        }
        let minus_one = self.ins(format!("icmp eq {ll} {b}, -1"));
        let safe = self.ins(format!("select i1 {minus_one}, {ll} 1, {ll} {b}"));
        let raw = self.ins(format!("{inst} {ll} {a}, {safe}"));
        let wrapped = if op == Op::Div {
            self.ins(format!("sub {ll} 0, {a}"))
        } else {
            "0".to_string()
        };
        Ok(self.ins(format!("select i1 {minus_one}, {ll} {wrapped}, {ll} {raw}")))
    }

    /// Shifts with Go's semantics: a count of at least the operand width
    /// gives 0 (or the sign fill), where LLVM would give poison.
    pub(super) fn shift(&mut self, op: Op, left: Val, right: Val) -> R<Val> {
        if left.ty.is_untyped_num()
            && let (Some(K::Int(a)), Some(K::Int(n))) = (left.k, right.k)
        {
            if !(0..=100).contains(&n) {
                return Err(format!("invalid shift count {n}"));
            }
            let v = if op == Op::Shl { a << n } else { a >> n };
            return Ok(Val::int(left.ty, v));
        }
        // `1 << n` with a variable count: the constant takes the type the
        // context expects, or int when there is none.
        let left = match self.shift_hint.clone() {
            Some(hint) if left.ty.is_untyped_num() => self.coerce(left, &hint)?,
            _ => self.materialize(left)?,
        };
        let Ty::Int { bits, signed } = *left.ty.under() else {
            return Err(format!("cannot shift {}", left.ty));
        };
        let ll = left.ty.ll();
        let inst = match (op, signed) {
            (Op::Shl, _) => "shl",
            (_, true) => "ashr",
            (_, false) => "lshr",
        };

        if right.ty.is_untyped_num() || right.k.is_some() {
            let Some(K::Int(n)) = right.k else {
                return Err("shift count must be an integer".into());
            };
            // A typed constant shifted by a constant stays a constant.
            if let Some(K::Int(a)) = left.k
                && (0..bits as i128).contains(&n)
            {
                let v = if op == Op::Shl { a << n } else { a >> n };
                let ty = left.ty.clone();
                return self.convert(Val::int(Ty::UntypedInt, v), &ty);
            }
            if n < 0 {
                return Err(format!("negative shift count {n}"));
            }
            if n >= bits as i128 {
                if inst != "ashr" {
                    return Ok(Val::int(left.ty, 0));
                }
                let name = self.ins(format!("ashr {ll} {}, {}", left.repr, bits - 1));
                return Ok(Val::new(left.ty, name));
            }
            let name = self.ins(format!("{inst} {ll} {}, {n}", left.repr));
            return Ok(Val::new(left.ty, name));
        }

        let Ty::Int {
            bits: count_bits, ..
        } = right.ty
        else {
            return Err(format!(
                "shift count must be an integer, found {}",
                right.ty
            ));
        };
        let count_ll = right.ty.ll();
        let too_big = self.ins(format!("icmp uge {count_ll} {}, {bits}", right.repr));
        let count = if count_bits > bits {
            self.ins(format!("trunc {count_ll} {} to {ll}", right.repr))
        } else if count_bits < bits {
            self.ins(format!("zext {count_ll} {} to {ll}", right.repr))
        } else {
            right.repr.clone()
        };
        let name = if inst == "ashr" {
            let clamped = self.ins(format!(
                "select i1 {too_big}, {ll} {}, {ll} {count}",
                bits - 1
            ));
            self.ins(format!("ashr {ll} {}, {clamped}", left.repr))
        } else {
            let raw = self.ins(format!("{inst} {ll} {}, {count}", left.repr));
            self.ins(format!("select i1 {too_big}, {ll} 0, {ll} {raw}"))
        };
        Ok(Val::new(left.ty, name))
    }

    pub(super) fn composite(&mut self, lit: &ast::CompositeLit, want: Option<&Ty>) -> R<Val> {
        let objs = self.objs;
        let ty = match &lit.typ {
            Some(Expr::Array(a)) if matches!(a.len, Some(Expr::Ellipsis(_))) => {
                Ty::Array(Box::new(self.resolve_type(&a.elt)?), lit.elts.len() as u64)
            }
            Some(t) => self.resolve_type(t)?,
            None => want
                .cloned()
                .ok_or("composite literal is missing its type")?,
        };
        match ty.under() {
            Ty::Struct(name) => {
                let fields = self.structs[name].clone();
                let ll = ty.ll();
                let mut cur = "zeroinitializer".to_string();
                for (i, elt) in lit.elts.iter().enumerate() {
                    let (index, value) = match elt {
                        Expr::KeyValue(kv) => {
                            let Expr::Ident(key) = &kv.key else {
                                return Err(format!("invalid field name in {name} literal"));
                            };
                            let field = objs.idents[*key].name.as_str();
                            let index = fields
                                .iter()
                                .position(|(n, _)| n == field)
                                .ok_or_else(|| format!("{name} has no field {field}"))?;
                            (index, &kv.val)
                        }
                        other => (i, other),
                    };
                    let Some((_, field_ty)) = fields.get(index) else {
                        return Err(format!("too many values in {name} literal"));
                    };
                    let val = self.expr_want(value, field_ty)?;
                    cur = self.ins(format!(
                        "insertvalue {ll} {cur}, {} {}, {index}",
                        field_ty.ll(),
                        val.repr
                    ));
                }
                Ok(Val::new(ty, cur))
            }
            Ty::Array(elem, n) => {
                if lit.elts.len() as u64 > *n {
                    return Err(format!("too many values in {ty} literal"));
                }
                let ll = ty.ll();
                let mut cur = "zeroinitializer".to_string();
                for (i, elt) in lit.elts.iter().enumerate() {
                    if matches!(elt, Expr::KeyValue(_)) {
                        return Err("indexed array literals are not supported yet".into());
                    }
                    let val = self.expr_want(elt, elem)?;
                    cur = self.ins(format!(
                        "insertvalue {ll} {cur}, {} {}, {i}",
                        elem.ll(),
                        val.repr
                    ));
                }
                Ok(Val::new(ty, cur))
            }
            Ty::Slice(elem) => {
                let n = lit.elts.len();
                if n == 0 {
                    return Ok(Val::new(ty, "zeroinitializer"));
                }
                let data = self.alloc_array(&n.to_string(), elem);
                for (i, elt) in lit.elts.iter().enumerate() {
                    if matches!(elt, Expr::KeyValue(_)) {
                        return Err("indexed slice literals are not supported yet".into());
                    }
                    let val = self.expr_want(elt, elem)?;
                    let slot = self.ins(format!(
                        "getelementptr inbounds {}, ptr {data}, i64 {i}",
                        elem.ll()
                    ));
                    self.store(&val, &slot);
                }
                let n = n.to_string();
                Ok(self.make_slice(ty.clone(), &data, &n, &n))
            }
            Ty::Map(key, val) => {
                let (key, val) = (key.as_ref().clone(), val.as_ref().clone());
                let map = self.map_new(&key, &val, &lit.elts.len().to_string());
                for elt in &lit.elts {
                    let Expr::KeyValue(kv) = elt else {
                        return Err("map literal entries must be key: value".into());
                    };
                    let k = self.expr_want(&kv.key, &key)?;
                    let v = self.expr_want(&kv.val, &val)?;
                    let key_slot = self.spill(&k);
                    let slot = self.ins(format!(
                        "call ptr @goiaba_map_assign(ptr {map}, ptr {key_slot})"
                    ));
                    self.store(&v, &slot);
                }
                Ok(Val::new(ty.clone(), map))
            }
            Ty::Ptr(inner) if lit.typ.is_none() => {
                // Elided `&T` inside a literal such as `[]*T{{...}}`.
                let val = self.composite(lit, Some(inner))?;
                let ptr = self.heap_alloc(&val.ty);
                self.store(&val, &ptr);
                Ok(Val::new(ty.clone(), ptr))
            }
            other => Err(format!("invalid composite literal type {other}")),
        }
    }

    pub(super) fn make_slice(&mut self, ty: Ty, data: &str, len: &str, cap: &str) -> Val {
        let a = self.ins(format!("insertvalue %slice poison, ptr {data}, 0"));
        let b = self.ins(format!("insertvalue %slice {a}, i64 {len}, 1"));
        let c = self.ins(format!("insertvalue %slice {b}, i64 {cap}, 2"));
        Val::new(ty, c)
    }

    pub(super) fn slice_expr(&mut self, s: &ast::SliceExpr) -> R<Val> {
        if s.slice3 || s.max.is_some() {
            return Err("3-index slices are not supported yet".into());
        }
        let base = match self.loc(&s.expr)? {
            Loc::Place(ptr, ty) if matches!(ty.under(), Ty::Array(..)) => {
                Loc::Place(ptr, ty.under().clone())
            }
            other => other,
        };
        let lo = match &s.low {
            Some(e) => {
                let v = self.expr(e)?;
                self.widen_i64(v)?
            }
            None => "0".to_string(),
        };
        let high = match &s.high {
            Some(e) => {
                let v = self.expr(e)?;
                Some(self.widen_i64(v)?)
            }
            None => None,
        };

        let from_array = |c: &mut Self, ptr: &str, elem: &Ty, n: u64, high: Option<String>| {
            let hi = high.unwrap_or_else(|| n.to_string());
            c.emit(format!(
                "call void @goiaba.slicecheck(i64 {lo}, i64 {hi}, i64 {n})"
            ));
            let data = c.ins(format!(
                "getelementptr inbounds {}, ptr {ptr}, i64 {lo}",
                elem.ll()
            ));
            let len = c.ins(format!("sub i64 {hi}, {lo}"));
            let cap = c.ins(format!("sub i64 {n}, {lo}"));
            c.make_slice(Ty::Slice(Box::new(elem.clone())), &data, &len, &cap)
        };

        let val = match base {
            Loc::Place(ptr, Ty::Array(elem, n)) => {
                self.mark_escape(&s.expr);
                return Ok(from_array(self, &ptr, &elem, n, high));
            }
            Loc::Place(ptr, ty) => self.load(&ptr, &ty),
            Loc::Value(val) => val,
        };
        match val.ty.under() {
            Ty::Slice(elem) => {
                let data = self.ins(format!("extractvalue %slice {}, 0", val.repr));
                let cap = self.ins(format!("extractvalue %slice {}, 2", val.repr));
                let hi = match high {
                    Some(hi) => hi,
                    None => self.ins(format!("extractvalue %slice {}, 1", val.repr)),
                };
                self.emit(format!(
                    "call void @goiaba.slicecheck(i64 {lo}, i64 {hi}, i64 {cap})"
                ));
                let new_data = self.ins(format!(
                    "getelementptr inbounds {}, ptr {data}, i64 {lo}",
                    elem.ll()
                ));
                let len = self.ins(format!("sub i64 {hi}, {lo}"));
                let new_cap = self.ins(format!("sub i64 {cap}, {lo}"));
                Ok(self.make_slice(val.ty.clone(), &new_data, &len, &new_cap))
            }
            Ty::Str => {
                let data = self.ins(format!("extractvalue %string {}, 0", val.repr));
                let len = self.ins(format!("extractvalue %string {}, 1", val.repr));
                let hi = high.unwrap_or_else(|| len.clone());
                self.emit(format!(
                    "call void @goiaba.slicecheck(i64 {lo}, i64 {hi}, i64 {len})"
                ));
                let new_data = self.ins(format!("getelementptr inbounds i8, ptr {data}, i64 {lo}"));
                let new_len = self.ins(format!("sub i64 {hi}, {lo}"));
                let a = self.ins(format!("insertvalue %string poison, ptr {new_data}, 0"));
                let b = self.ins(format!("insertvalue %string {a}, i64 {new_len}, 1"));
                Ok(Val::new(Ty::Str, b))
            }
            Ty::Ptr(inner) => match inner.as_ref() {
                Ty::Array(elem, n) => {
                    self.nilcheck(&val.repr);
                    Ok(from_array(self, &val.repr, elem, *n, high))
                }
                other => Err(format!("cannot slice *{other}")),
            },
            Ty::Array(_, _) => Err("cannot slice an array that is not addressable".into()),
            other => Err(format!("cannot slice {other}")),
        }
    }
}
