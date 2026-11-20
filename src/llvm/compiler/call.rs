// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! Calls: user functions, methods, function values and builtins.

use super::*;

impl<'a> Compiler<'a> {
    pub(super) fn call(&mut self, call: &ast::CallExpr) -> R<Val> {
        self.call_mode(call, false)
    }

    /// Lowers a call. With `capture` the call is evaluated but saved in
    /// `self.f.pending` instead of emitted; only user functions, methods and
    /// function values can be captured.
    pub(super) fn call_mode(&mut self, call: &ast::CallExpr, capture: bool) -> R<Val> {
        let objs = self.objs;
        match strip_parens(&call.func) {
            Expr::Ident(key) => {
                let name = objs.idents[*key].name.as_str();
                if self.lookup(name).is_some() {
                    let callee = self.expr(&call.func)?;
                    return self.call_value(name, callee, call, capture);
                }
                if let Some(sig) = self.funcs.get(&self.q(name)).cloned() {
                    return self.user_call(name, &sig, None, call, capture);
                }
                match name {
                    "len" | "cap" => self.len_cap(name, call),
                    "append" => self.append(call),
                    "close" => {
                        let [arg] = call.args.as_slice() else {
                            return Err("close takes exactly one channel".into());
                        };
                        let chan = self.expr(arg)?;
                        if !matches!(chan.ty.under(), Ty::Chan(_)) {
                            return Err(format!("cannot close {}", chan.ty));
                        }
                        self.emit(format!("call void @goiaba_chan_close(ptr {})", chan.repr));
                        Ok(Val::void())
                    }
                    "delete" => {
                        let [map, key] = call.args.as_slice() else {
                            return Err("delete takes a map and a key".into());
                        };
                        let map = self.expr(map)?;
                        let Ty::Map(key_ty, _) = map.ty.under().clone() else {
                            return Err(format!("cannot delete from {}", map.ty));
                        };
                        let key = self.expr_want(key, &key_ty)?;
                        let slot = self.spill(&key);
                        self.emit(format!(
                            "call void @goiaba_map_delete(ptr {}, ptr {slot})",
                            map.repr
                        ));
                        Ok(Val::void())
                    }
                    "make" => self.make(call),
                    "new" => {
                        let [arg] = call.args.as_slice() else {
                            return Err("new takes exactly one type".into());
                        };
                        let ty = self.resolve_type(arg)?;
                        let ptr = self.heap_alloc(&ty);
                        Ok(Val::new(Ty::Ptr(Box::new(ty)), ptr))
                    }
                    "println" => self.print(&call.args, true),
                    "print" => self.print(&call.args, false),
                    "panic" => self.panic(call),
                    "recover" => {
                        let any = Ty::Iface(Vec::new());
                        let out = self.alloca(&any);
                        self.emit(format!("call void @goiaba_recover(ptr {out})"));
                        Ok(self.load(&out, &any))
                    }
                    _ => match self.resolve_type(&call.func) {
                        Ok(ty) => self.conversion(&ty, call),
                        Err(_) => Err(format!("undefined: {name}")),
                    },
                }
            }
            Expr::Selector(s) => self.method_call(s, call, capture),
            Expr::Array(_) | Expr::Star(_) | Expr::Func(_) | Expr::Map(_) | Expr::Interface(_) => {
                let ty = self.resolve_type(&call.func)?;
                self.conversion(&ty, call)
            }
            _ => {
                let callee = self.expr(&call.func)?;
                self.call_value("function value", callee, call, capture)
            }
        }
    }

    /// Calls a function value: `code(ctx, args...)`.
    pub(super) fn call_value(
        &mut self,
        name: &str,
        callee: Val,
        call: &ast::CallExpr,
        capture: bool,
    ) -> R<Val> {
        let Ty::Func(sig) = callee.ty.under().clone() else {
            return Err(format!("cannot call a value of type {}", callee.ty));
        };
        let code = self.ins(format!("extractvalue %func {}, 0", callee.repr));
        let ctx = self.ins(format!("extractvalue %func {}, 1", callee.repr));
        self.nilcheck(&code);
        let mut args = vec![("ptr".to_string(), code), ("ptr".to_string(), ctx)];
        args.extend(self.lower_args(name, &sig.params, sig.variadic, call)?);
        Ok(self.finish_call(&sig.ret, None, args, capture))
    }

    pub(super) fn conversion(&mut self, ty: &Ty, call: &ast::CallExpr) -> R<Val> {
        let [arg] = call.args.as_slice() else {
            return Err(format!("conversion to {ty} takes exactly one argument"));
        };
        let val = self.expr(arg)?;
        self.convert(val, ty)
    }

    pub(super) fn user_call(
        &mut self,
        name: &str,
        sig: &FuncSig,
        receiver: Option<Val>,
        call: &ast::CallExpr,
        capture: bool,
    ) -> R<Val> {
        let mut args = Vec::new();
        let mut params = sig.params.as_slice();
        if let Some(recv) = receiver {
            params = &params[1..];
            args.push((recv.ty.ll(), recv.repr));
        }
        args.extend(self.lower_args(name, params, sig.variadic, call)?);
        if sig.extern_c {
            let mut scalars = Vec::new();
            for (ty, value) in args {
                let parts: &[&str] = match ty.as_str() {
                    "%string" => &["ptr", "i64"],
                    "%slice" => &["ptr", "i64", "i64"],
                    "%iface" => &["ptr", "ptr"],
                    _ => &[],
                };
                if parts.is_empty() {
                    scalars.push((ty, value));
                    continue;
                }
                for (i, part) in parts.iter().enumerate() {
                    let word = self.ins(format!("extractvalue {ty} {value}, {i}"));
                    scalars.push((part.to_string(), word));
                }
            }
            args = scalars;
        }
        let symbol = format!("@\"{}\"", sig.symbol);
        Ok(self.finish_call(&sig.ret, Some(symbol), args, capture))
    }

    /// Evaluates call arguments against `params` and returns them as typed
    /// LLVM operands. Extra arguments of a variadic call are packed in a slice.
    pub(super) fn lower_args(
        &mut self,
        name: &str,
        params: &[Ty],
        variadic: bool,
        call: &ast::CallExpr,
    ) -> R<Vec<(String, String)>> {
        let mut args = Vec::new();
        let in_call = |e: String| format!("in call to {name}: {e}");
        let spread = call.ellipsis.is_some();
        if spread && !variadic {
            return Err(format!(
                "{name}: cannot use ... in a call to a non-variadic function"
            ));
        }
        // `f(g())` where g returns several values: they become f's arguments.
        if !variadic
            && params.len() > 1
            && call.args.len() == 1
            && matches!(strip_parens(&call.args[0]), Expr::Call(_))
        {
            let val = self.expr(&call.args[0])?;
            let ll = val.ty.ll();
            if let Ty::Tuple(items) = &val.ty
                && items.len() == params.len()
            {
                for (i, (item, param)) in items.iter().zip(params).enumerate() {
                    let part = self.ins(format!("extractvalue {ll} {}, {i}", val.repr));
                    let part = self
                        .coerce(Val::new(item.clone(), part), param)
                        .map_err(in_call)?;
                    args.push((param.ll(), part.repr));
                }
                return Ok(args);
            }
            return Err(format!(
                "{name}: expected {} arguments, found 1",
                params.len()
            ));
        }
        let fixed = if variadic {
            params.len() - 1
        } else {
            params.len()
        };
        let count_ok = if spread {
            call.args.len() == params.len()
        } else if variadic {
            call.args.len() >= fixed
        } else {
            call.args.len() == fixed
        };
        if !count_ok {
            return Err(format!(
                "{name}: expected {} arguments, found {}",
                params.len(),
                call.args.len()
            ));
        }
        for (param, arg) in params[..fixed].iter().zip(&call.args) {
            let val = self.expr_want(arg, param).map_err(in_call)?;
            args.push((param.ll(), val.repr));
        }
        if variadic {
            let slice_ty = &params[fixed];
            let Ty::Slice(elem) = slice_ty.under() else {
                return Err("internal: variadic parameter is not a slice".into());
            };
            let rest = &call.args[fixed..];
            let packed = if spread {
                self.expr_want(&rest[0], slice_ty).map_err(in_call)?
            } else if rest.is_empty() {
                Val::new(slice_ty.clone(), "zeroinitializer")
            } else {
                let n = rest.len().to_string();
                let data = self.alloc_array(&n, elem);
                for (i, arg) in rest.iter().enumerate() {
                    let val = self.expr_want(arg, elem).map_err(in_call)?;
                    let slot = self.ins(format!(
                        "getelementptr inbounds {}, ptr {data}, i64 {i}",
                        elem.ll()
                    ));
                    self.store(&val, &slot);
                }
                self.make_slice(slice_ty.clone(), &data, &n, &n)
            };
            args.push((slice_ty.ll(), packed.repr));
        }
        Ok(args)
    }

    pub(super) fn method_call(
        &mut self,
        s: &ast::SelectorExpr,
        call: &ast::CallExpr,
        capture: bool,
    ) -> R<Val> {
        let objs = self.objs;
        let method = objs.idents[s.sel].name.as_str();
        if let Some(package) = self.package_of(&s.expr) {
            // `pkg.Func(args)`, or the conversion `pkg.Type(x)`.
            let key = format!("{package}.{method}");
            if let Some(sig) = self.funcs.get(&key).cloned() {
                return self.user_call(&key, &sig, None, call, capture);
            }
            if let Ok(ty) = self.resolve_type(&call.func) {
                return self.conversion(&ty, call);
            }
            return match self.globals.get(&key).cloned() {
                Some(_) => {
                    let callee = self.expr(&call.func)?;
                    self.call_value(&key, callee, call, capture)
                }
                None => Err(format!("undefined: {key}")),
            };
        }

        let recv = self.loc(&s.expr)?;
        let recv = self.promoted_receiver(recv, method)?;
        let recv_ty = match &recv {
            Loc::Place(_, ty) => ty.clone(),
            Loc::Value(val) => val.ty.clone(),
        };
        // Methods are declared on T; a *T receiver expression reaches them too.
        let (base, recv_is_ptr) = match &recv_ty {
            Ty::Ptr(inner) => (inner.as_ref().clone(), true),
            other => (other.clone(), false),
        };
        if let Ty::Iface(methods) = recv_ty.under() {
            let Some(index) = methods.iter().position(|(name, _)| name == method) else {
                return Err(format!("{recv_ty} has no method {method}"));
            };
            let sig = methods[index].1.clone();
            let val = match recv {
                Loc::Place(ptr, ty) => self.load(&ptr, &ty),
                Loc::Value(val) => val,
            };
            let tab = self.ins(format!("extractvalue %iface {}, 0", val.repr));
            let data = self.ins(format!("extractvalue %iface {}, 1", val.repr));
            self.nilcheck(&tab);
            let slot = self.ins(format!(
                "getelementptr inbounds ptr, ptr {tab}, i64 {}",
                index + 1
            ));
            let code = self.ins(format!("load ptr, ptr {slot}"));
            let mut args = vec![("ptr".to_string(), code), ("ptr".to_string(), data)];
            args.extend(self.lower_args(method, &sig.params, sig.variadic, call)?);
            return Ok(self.finish_call(&sig.ret, None, args, capture));
        }
        let key = base.type_name().map(|name| format!("{name}.{method}"));
        let sig = key.as_ref().and_then(|key| self.funcs.get(key).cloned());
        let (Some(key), Some(sig)) = (key, sig) else {
            // Not a method: a struct field that holds a function value.
            let callee = match self.field_of(recv, method)? {
                Loc::Place(ptr, ty) => self.load(&ptr, &ty),
                Loc::Value(val) => val,
            };
            return self.call_value(method, callee, call, capture);
        };

        // Adjust the receiver to what the method declares, as Go does implicitly.
        let receiver = if sig.ptr_recv == Some(true) {
            match recv {
                Loc::Place(ptr, ty) if !recv_is_ptr => {
                    self.mark_escape(&s.expr);
                    Val::new(Ty::Ptr(Box::new(ty)), ptr)
                }
                Loc::Place(ptr, ty) => self.load(&ptr, &ty),
                Loc::Value(val) if recv_is_ptr => val,
                Loc::Value(val) => {
                    let slot = self.spill(&val);
                    Val::new(Ty::Ptr(Box::new(val.ty)), slot)
                }
            }
        } else {
            let val = match recv {
                Loc::Place(ptr, ty) => self.load(&ptr, &ty),
                Loc::Value(val) => val,
            };
            if recv_is_ptr {
                self.nilcheck(&val.repr);
                self.load(&val.repr, &base)
            } else {
                val
            }
        };
        self.user_call(&key, &sig, Some(receiver), call, capture)
    }

    pub(super) fn len_cap(&mut self, name: &str, call: &ast::CallExpr) -> R<Val> {
        let [arg] = call.args.as_slice() else {
            return Err(format!("{name} takes exactly one argument"));
        };
        let val = self.expr_m(arg)?;
        let array_len = |ty: &Ty| match ty {
            Ty::Array(_, n) => Some(*n),
            _ => None,
        };
        match val.ty.under() {
            Ty::Str if name == "len" => {
                let n = self.ins(format!("extractvalue %string {}, 1", val.repr));
                Ok(Val::new(INT, n))
            }
            Ty::Slice(_) => {
                let index = if name == "len" { 1 } else { 2 };
                let n = self.ins(format!("extractvalue %slice {}, {index}", val.repr));
                Ok(Val::new(INT, n))
            }
            Ty::Chan(_) => {
                let n = self.ins(format!("call i64 @goiaba_chan_{name}(ptr {})", val.repr));
                Ok(Val::new(INT, n))
            }
            Ty::Map(..) if name == "len" => {
                let n = self.ins(format!("call i64 @goiaba_map_len(ptr {})", val.repr));
                Ok(Val::new(INT, n))
            }
            Ty::Array(_, n) => Ok(Val::int(INT, *n as i128)),
            Ty::Ptr(inner) if array_len(inner).is_some() => {
                Ok(Val::int(INT, array_len(inner).unwrap() as i128))
            }
            other => Err(format!("invalid argument of type {other} for {name}")),
        }
    }

    pub(super) fn make(&mut self, call: &ast::CallExpr) -> R<Val> {
        let Some(type_expr) = call.args.first() else {
            return Err("make needs a type".into());
        };
        let ty = self.resolve_type(type_expr)?;
        if let Ty::Chan(elem) = ty.under() {
            let cap = match call.args.get(1) {
                Some(e) => {
                    let v = self.expr(e)?;
                    self.widen_i64(v)?
                }
                None => "0".to_string(),
            };
            let chan = self.ins(format!(
                "call ptr @goiaba_chan_new(i64 {}, i64 {cap})",
                sizeof(elem)
            ));
            return Ok(Val::new(ty, chan));
        }
        if let Ty::Map(key, val) = ty.under() {
            let hint = match call.args.get(1) {
                Some(e) => {
                    let v = self.expr(e)?;
                    self.widen_i64(v)?
                }
                None => "0".to_string(),
            };
            let (key, val) = (key.as_ref().clone(), val.as_ref().clone());
            let map = self.map_new(&key, &val, &hint);
            return Ok(Val::new(ty, map));
        }
        let Ty::Slice(elem) = ty.under() else {
            return Err(format!("make({ty}) is not supported yet"));
        };
        if call.args.len() < 2 || call.args.len() > 3 {
            return Err("make([]T, len[, cap]) takes a length and an optional capacity".into());
        }
        let len_val = self.expr(&call.args[1])?;
        let len = self.widen_i64(len_val)?;
        let cap = match call.args.get(2) {
            Some(e) => {
                let v = self.expr(e)?;
                self.widen_i64(v)?
            }
            None => len.clone(),
        };
        let negative = self.ins(format!("icmp slt i64 {cap}, 0"));
        let too_long = self.ins(format!("icmp ugt i64 {len}, {cap}"));
        let bad = self.ins(format!("or i1 {negative}, {too_long}"));
        self.panic_if(&bad, "makeslice: len out of range");
        let data = self.alloc_array(&cap, elem);
        Ok(self.make_slice(ty.clone(), &data, &len, &cap))
    }

    pub(super) fn append(&mut self, call: &ast::CallExpr) -> R<Val> {
        let Some(first) = call.args.first() else {
            return Err("append needs a slice".into());
        };
        let mut slice = self.expr(first)?;
        let Ty::Slice(elem) = slice.ty.under().clone() else {
            return Err(format!(
                "first argument to append must be a slice, found {}",
                slice.ty
            ));
        };
        let size = sizeof(&elem);
        if call.ellipsis.is_some() {
            // append(s, t...): t is a slice of the same element type, or a
            // string when appending to a []byte.
            let [_, rest] = call.args.as_slice() else {
                return Err("append(s, t...) takes exactly two arguments".into());
            };
            let rest = self.expr_m(rest)?;
            let kind = match rest.ty.under() {
                Ty::Slice(other) if *other == elem => "%slice",
                Ty::Str
                    if *elem
                        == (Ty::Int {
                            bits: 8,
                            signed: false,
                        }) =>
                {
                    "%string"
                }
                _ => return Err(format!("cannot append {}... to {}", rest.ty, slice.ty)),
            };
            let data = self.ins(format!("extractvalue {kind} {}, 0", rest.repr));
            let len = self.ins(format!("extractvalue {kind} {}, 1", rest.repr));
            let name = self.ins(format!(
                "call %slice @goiaba.append(%slice {}, ptr {data}, i64 {len}, i64 {size})",
                slice.repr
            ));
            return Ok(Val::new(slice.ty, name));
        }
        for arg in &call.args[1..] {
            let val = self.expr_want(arg, &elem)?;
            let slot = self.spill(&val);
            let name = self.ins(format!(
                "call %slice @goiaba.append(%slice {}, ptr {slot}, i64 1, i64 {size})",
                slice.repr
            ));
            slice = Val::new(slice.ty, name);
        }
        Ok(slice)
    }

    /// Prints one value the way Go's `print` builtin does.
    pub(super) fn print_val(&mut self, val: &Val) -> R<()> {
        match val.ty.under() {
            Ty::Int { signed, .. } => {
                let signed = *signed;
                let wide = self.convert(val.clone(), &Ty::Int { bits: 64, signed })?;
                let callee = if signed {
                    "goiaba_print_int"
                } else {
                    "goiaba_print_uint"
                };
                self.emit(format!("call void @{callee}(i64 {})", wide.repr));
            }
            Ty::Float(_) => {
                let wide = self.convert(val.clone(), &Ty::Float(64))?;
                self.emit(format!(
                    "call void @goiaba_print_float(double {})",
                    wide.repr
                ));
            }
            Ty::Bool => {
                let wide = self.ins(format!("zext i1 {} to i32", val.repr));
                self.emit(format!("call void @goiaba_print_bool(i32 {wide})"));
            }
            Ty::Str => {
                let data = self.ins(format!("extractvalue %string {}, 0", val.repr));
                let len = self.ins(format!("extractvalue %string {}, 1", val.repr));
                self.emit(format!(
                    "call void @goiaba_print_string(ptr {data}, i64 {len})"
                ));
            }
            Ty::Ptr(_) | Ty::Map(..) | Ty::Chan(_) => {
                self.emit(format!("call void @goiaba_print_pointer(ptr {})", val.repr));
            }
            Ty::Iface(_) => {
                // Like Go: the two words of the interface value.
                self.print_text("(");
                for (i, sep) in [",", ")"].iter().enumerate() {
                    let word = self.ins(format!("extractvalue %iface {}, {i}", val.repr));
                    self.emit(format!("call void @goiaba_print_pointer(ptr {word})"));
                    self.print_text(sep);
                }
            }
            _ => return Err(format!("cannot print a value of type {}", val.ty)),
        }
        Ok(())
    }

    pub(super) fn print_text(&mut self, text: &str) {
        let global = self.intern(text.as_bytes().to_vec());
        self.emit(format!(
            "call void @goiaba_print_string(ptr {global}, i64 {})",
            text.len()
        ));
    }

    /// `print` and `println` write to standard error, like the Go builtins.
    pub(super) fn print(&mut self, args: &[Expr], newline: bool) -> R<Val> {
        // Every argument is evaluated before anything is written.
        let vals = args
            .iter()
            .map(|arg| self.expr_m(arg))
            .collect::<R<Vec<_>>>()?;
        for (i, val) in vals.iter().enumerate() {
            if newline && i > 0 {
                self.print_text(" ");
            }
            self.print_val(val)?;
        }
        if newline {
            self.print_text("\n");
        }
        Ok(Val::void())
    }

    /// `panic(v)`: v becomes an `any` value handed to the runtime.
    pub(super) fn panic(&mut self, call: &ast::CallExpr) -> R<Val> {
        let [arg] = call.args.as_slice() else {
            return Err("panic takes exactly one argument".into());
        };
        let any = Ty::Iface(Vec::new());
        let val = self.expr_want(arg, &any)?;
        let rtype = self.ins(format!("extractvalue %iface {}, 0", val.repr));
        let data = self.ins(format!("extractvalue %iface {}, 1", val.repr));
        self.emit(format!("call void @goiaba_panic(ptr {rtype}, ptr {data})"));
        self.emit("unreachable".into());
        self.f.terminated = true;
        Ok(Val::void())
    }
}
