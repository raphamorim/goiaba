// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! Type declarations and type resolution: structs, embedding, signatures.

use super::*;

impl<'a> Compiler<'a> {
    /// Registers `type Name T`. An alias (`type Name = T`) keeps T's identity.
    pub(super) fn define_type(&mut self, ts: &ast::TypeSpec) -> R<()> {
        let name = self.q(&self.objs.idents[ts.name].name);
        let ty = self.resolve_type(&ts.typ)?;
        let ty = if ts.assign > 0 {
            ty
        } else {
            Ty::Named(name.clone(), Box::new(ty))
        };
        self.aliases.insert(name, ty);
        Ok(())
    }

    pub(super) fn declare_struct_name(&mut self, name: &str) {
        if !self.structs.contains_key(name) {
            self.structs.insert(name.to_string(), Vec::new());
            self.struct_order.push(name.to_string());
        }
    }

    /// Resolves a struct's fields and records them under `name`. An embedded
    /// field is named after its type.
    pub(super) fn define_struct(&mut self, name: &str, st: &ast::StructType) -> R<()> {
        let (fields, embedded) = self.struct_layout(st)?;
        self.structs.insert(name.to_string(), fields);
        self.embedded.insert(name.to_string(), embedded);
        Ok(())
    }

    /// An anonymous `struct { ... }` type. Identical field lists share one
    /// type, named after its fields.
    pub(super) fn anonymous_struct(&mut self, st: &ast::StructType) -> R<Ty> {
        let (fields, embedded) = self.struct_layout(st)?;
        let parts: Vec<String> = fields
            .iter()
            .map(|(name, ty)| format!("{name} {}", ty.go_name()))
            .collect();
        let name = if parts.is_empty() {
            "struct {}".to_string()
        } else {
            format!("struct {{ {} }}", parts.join("; "))
        };
        if !self.structs.contains_key(&name) {
            self.struct_order.push(name.clone());
            self.structs.insert(name.clone(), fields);
            self.embedded.insert(name.clone(), embedded);
        }
        Ok(Ty::Struct(name))
    }

    pub(super) fn struct_layout(&mut self, st: &ast::StructType) -> R<StructLayout> {
        let objs = self.objs;
        let mut fields = Vec::new();
        let mut embedded = Vec::new();
        for key in &st.fields.list {
            let field = &objs.fields[*key];
            let ty = self.resolve_type(&field.typ)?;
            if field.names.is_empty() {
                let mut type_expr = strip_parens(&field.typ);
                if let Expr::Star(star) = type_expr {
                    type_expr = strip_parens(&star.expr);
                }
                let field_name = match type_expr {
                    Expr::Ident(key) => objs.idents[*key].name.clone(),
                    Expr::Selector(s) => objs.idents[s.sel].name.clone(),
                    _ => return Err("invalid embedded field".into()),
                };
                embedded.push(fields.len());
                fields.push((field_name, ty));
                continue;
            }
            for field_name in &field.names {
                fields.push((objs.idents[*field_name].name.clone(), ty.clone()));
            }
        }
        Ok((fields, embedded))
    }

    /// The field indexes that lead from a struct to the field `sel`, which
    /// may be promoted from an embedded struct. Shallower fields win.
    pub(super) fn field_path(&self, struct_name: &str, sel: &str) -> Option<Vec<usize>> {
        let mut queue = std::collections::VecDeque::from([(struct_name.to_string(), Vec::new())]);
        let mut seen = HashSet::new();
        while let Some((name, path)) = queue.pop_front() {
            if !seen.insert(name.clone()) {
                continue;
            }
            let fields = self.structs.get(&name)?;
            if let Some(index) = fields.iter().position(|(field, _)| field == sel) {
                let mut path = path;
                path.push(index);
                return Some(path);
            }
            for index in self.embedded.get(&name).into_iter().flatten() {
                let inner = match &fields[*index].1 {
                    Ty::Ptr(inner) => inner.as_ref(),
                    other => other,
                };
                if let Ty::Struct(inner_name) = inner.under() {
                    let mut next = path.clone();
                    next.push(*index);
                    queue.push_back((inner_name.clone(), next));
                }
            }
        }
        None
    }

    /// The field indexes that lead from a struct to the embedded field whose
    /// type declares `method`. `None` when no embedded type has it.
    pub(super) fn method_path(&self, struct_name: &str, method: &str) -> Option<Vec<usize>> {
        let mut queue = std::collections::VecDeque::from([(struct_name.to_string(), Vec::new())]);
        let mut seen = HashSet::new();
        while let Some((name, path)) = queue.pop_front() {
            if !seen.insert(name.clone()) {
                continue;
            }
            let fields = self.structs.get(&name)?;
            for index in self.embedded.get(&name).into_iter().flatten() {
                let inner = match &fields[*index].1 {
                    Ty::Ptr(inner) => inner.as_ref(),
                    other => other,
                };
                let mut next = path.clone();
                next.push(*index);
                let has_method = match inner.under() {
                    Ty::Iface(methods) => methods.iter().any(|(name, _)| name == method),
                    _ => inner
                        .type_name()
                        .is_some_and(|t| self.funcs.contains_key(&format!("{t}.{method}"))),
                };
                if has_method {
                    return Some(next);
                }
                if let Ty::Struct(inner_name) = inner.under() {
                    queue.push_back((inner_name.clone(), next));
                }
            }
        }
        None
    }

    /// When `method` is promoted from an embedded field of `recv`, steps into
    /// that field and returns it as the receiver.
    pub(super) fn promoted_receiver(&mut self, recv: Loc, method: &str) -> R<Loc> {
        let recv_ty = match &recv {
            Loc::Place(_, ty) => ty,
            Loc::Value(val) => &val.ty,
        };
        let base = match recv_ty {
            Ty::Ptr(inner) => inner.as_ref(),
            other => other,
        };
        if base
            .type_name()
            .is_some_and(|t| self.funcs.contains_key(&format!("{t}.{method}")))
        {
            return Ok(recv);
        }
        let Ty::Struct(struct_name) = base.under() else {
            return Ok(recv);
        };
        let Some(path) = self.method_path(struct_name, method) else {
            return Ok(recv);
        };
        self.walk_fields(recv, &path)
    }

    /// Follows field indexes from a struct value, through embedded pointers.
    pub(super) fn walk_fields(&mut self, base: Loc, path: &[usize]) -> R<Loc> {
        let mut loc = base;
        for index in path {
            let (ptr, struct_name) = self.struct_ptr(loc)?;
            let field_ty = self.structs[&struct_name][*index].1.clone();
            let place = self.ins(format!(
                "getelementptr inbounds {}, ptr {ptr}, i32 0, i32 {index}",
                Ty::Struct(struct_name).ll()
            ));
            loc = Loc::Place(place, field_ty);
        }
        Ok(loc)
    }

    pub(super) fn resolve_type(&mut self, e: &Expr) -> R<Ty> {
        let objs = self.objs;
        match e {
            Expr::Paren(p) => self.resolve_type(&p.expr),
            Expr::Ident(key) => {
                let name = objs.idents[*key].name.as_str();
                let key = self.q(name);
                if self.structs.contains_key(&key) {
                    Ok(Ty::Struct(key))
                } else if let Some(ty) = self.aliases.get(&key) {
                    Ok(ty.clone())
                } else if let Some(ty) = builtin_type(name) {
                    Ok(ty)
                } else {
                    Err(format!("undefined type: {name}"))
                }
            }
            Expr::Star(s) => Ok(Ty::Ptr(Box::new(self.resolve_type(&s.expr)?))),
            Expr::Array(a) => {
                let elem = Box::new(self.resolve_type(&a.elt)?);
                match &a.len {
                    None => Ok(Ty::Slice(elem)),
                    Some(Expr::Ellipsis(_)) => {
                        Err("[...]T is only allowed in a composite literal".into())
                    }
                    Some(len) => match self.expr(len)?.k {
                        Some(K::Int(n)) if n >= 0 => Ok(Ty::Array(elem, n as u64)),
                        _ => Err("array length must be a non-negative integer constant".into()),
                    },
                }
            }
            Expr::Struct(st) => self.anonymous_struct(st),
            Expr::Map(m) => {
                let key = self.resolve_type(&m.key)?;
                let val = self.resolve_type(&m.val)?;
                if !matches!(
                    key.under(),
                    Ty::Int { .. } | Ty::Bool | Ty::Float(_) | Ty::Str | Ty::Ptr(_)
                ) {
                    return Err(format!("map key type {key} is not supported yet"));
                }
                Ok(Ty::Map(Box::new(key), Box::new(val)))
            }
            Expr::Chan(c) => Ok(Ty::Chan(Box::new(self.resolve_type(&c.val)?))),
            Expr::Interface(it) => {
                let mut methods = Vec::new();
                for field_key in &it.methods.list {
                    let field = &objs.fields[*field_key];
                    if field.names.is_empty() {
                        // An embedded interface contributes its methods.
                        let embedded = self.resolve_type(&field.typ)?;
                        let Ty::Iface(inner) = embedded.under() else {
                            return Err(format!("{embedded} is not an interface"));
                        };
                        methods.extend(inner.iter().cloned());
                        continue;
                    }
                    let Expr::Func(key) = &field.typ else {
                        return Err("invalid interface method".into());
                    };
                    let sig = self.signature(&objs.ftypes[*key])?.func_ty();
                    for name in &field.names {
                        methods.push((objs.idents[*name].name.clone(), sig.clone()));
                    }
                }
                methods.sort_by(|a, b| a.0.cmp(&b.0));
                methods.dedup_by(|a, b| a.0 == b.0);
                Ok(Ty::Iface(methods))
            }
            Expr::Func(key) => {
                let sig = self.signature(&objs.ftypes[*key])?;
                Ok(Ty::Func(Box::new(sig.func_ty())))
            }
            Expr::Selector(s) => {
                let Some(package) = self.package_of(&s.expr) else {
                    return Err("expression is not a type".into());
                };
                let key = format!("{package}.{}", objs.idents[s.sel].name);
                if self.structs.contains_key(&key) {
                    Ok(Ty::Struct(key))
                } else if let Some(ty) = self.aliases.get(&key) {
                    Ok(ty.clone())
                } else {
                    Err(format!("undefined type: {key}"))
                }
            }
            _ => Err("expression is not a type".into()),
        }
    }

    /// Resolves the parameter and result types of a function type.
    pub(super) fn signature(&mut self, ft: &ast::FuncType) -> R<Signature> {
        let objs = self.objs;
        let mut params = Vec::new();
        let mut variadic = false;
        for field_key in &ft.params.list {
            let field = &objs.fields[*field_key];
            if variadic {
                return Err("only the last parameter can be variadic".into());
            }
            let ty = match &field.typ {
                Expr::Ellipsis(e) => {
                    variadic = true;
                    let elem = e.elt.as_ref().ok_or("variadic parameter needs a type")?;
                    Ty::Slice(Box::new(self.resolve_type(elem)?))
                }
                other => self.resolve_type(other)?,
            };
            if field.names.is_empty() {
                params.push(("_".to_string(), ty));
            } else {
                for n in &field.names {
                    params.push((objs.idents[*n].name.clone(), ty.clone()));
                }
            }
        }

        let mut result_types = Vec::new();
        let mut results = Vec::new();
        if let Some(list) = &ft.results {
            for field_key in &list.list {
                let field = &objs.fields[*field_key];
                let ty = self.resolve_type(&field.typ)?;
                if field.names.is_empty() {
                    result_types.push(ty);
                } else {
                    for n in &field.names {
                        results.push(objs.idents[*n].name.clone());
                        result_types.push(ty.clone());
                    }
                }
            }
        }
        if !results.is_empty() && results.len() != result_types.len() {
            return Err("cannot mix named and unnamed results".into());
        }
        let ret = match result_types.len() {
            0 => Ty::Void,
            1 => result_types.pop().unwrap(),
            _ => Ty::Tuple(result_types),
        };
        Ok(Signature {
            params,
            results,
            ret,
            variadic,
        })
    }

    pub(super) fn func_shape(&mut self, fd: &ast::FuncDecl) -> R<Shape> {
        let objs = self.objs;
        let name = objs.idents[fd.name].name.clone();
        if objs.ftypes[fd.typ].typ_params.is_some() {
            return Err(format!(
                "{name}: generic functions are parsed but not supported by the LLVM backend yet"
            ));
        }
        let mut sig = self.signature(&objs.ftypes[fd.typ])?;
        let mut key = self.q(&name);
        let mut ptr_recv = None;

        if let Some(recv) = &fd.recv {
            let Some(field_key) = recv.list.first() else {
                return Err(format!("{name}: method has no receiver"));
            };
            let field = &objs.fields[*field_key];
            let ty = self.resolve_type(&field.typ)?;
            let (base, is_ptr) = match &ty {
                Ty::Ptr(inner) => (inner.as_ref(), true),
                other => (other, false),
            };
            let Some(type_name) = base.type_name() else {
                return Err(format!("{name}: invalid receiver type {ty}"));
            };
            key = format!("{type_name}.{name}");
            ptr_recv = Some(is_ptr);
            let recv_name = match field.names.first() {
                Some(n) => objs.idents[*n].name.clone(),
                None => "_".to_string(),
            };
            sig.params.insert(0, (recv_name, ty));
        }

        Ok(Shape { key, sig, ptr_recv })
    }
}
