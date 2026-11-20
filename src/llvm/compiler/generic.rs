// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! Generics, compiled by instantiation: every distinct list of type
//! arguments gets its own copy of a generic function or type.
//!
//! Using an instance registers its signature at once, so calls can be
//! lowered, and queues its body. Bodies are lowered at the end of each
//! package, in the package that declares the generic, with the type
//! parameters bound to the type arguments in `type_env`.
//!
//! Constraints are not checked. An operation that the type argument does not
//! support is reported when the instance's body is lowered.

use super::*;

/// The package a declaration belongs to, so it can be lowered later.
#[derive(Clone)]
pub(super) struct Ctx<'a> {
    objs: &'a AstObjects,
    files: &'a [SourceFile],
    pkg: String,
    packages: HashMap<String, String>,
}

pub(super) struct GenericFunc<'a> {
    fd: &'a ast::FuncDecl,
    ctx: Ctx<'a>,
}

pub(super) struct GenericType<'a> {
    ts: &'a ast::TypeSpec,
    ctx: Ctx<'a>,
    pub(super) methods: Vec<&'a ast::FuncDecl>,
}

/// The body of an instance that still has to be lowered.
pub(super) struct PendingBody<'a> {
    fd: &'a ast::FuncDecl,
    key: String,
    ctx: Ctx<'a>,
    env: HashMap<String, Ty>,
}

/// The names declared by a type parameter list.
fn param_names(objs: &AstObjects, list: &ast::FieldList) -> Vec<String> {
    list.list
        .iter()
        .flat_map(|key| &objs.fields[*key].names)
        .map(|name| objs.idents[*name].name.clone())
        .collect()
}

/// The type of each parameter, one entry per parameter name.
fn param_types<'a>(objs: &'a AstObjects, list: &ast::FieldList) -> Vec<&'a Expr> {
    list.list
        .iter()
        .flat_map(|key| {
            let field = &objs.fields[*key];
            std::iter::repeat_n(&field.typ, field.names.len().max(1))
        })
        .collect()
}

fn instance_key(key: &str, args: &[Ty]) -> String {
    let parts: Vec<String> = args.iter().map(|ty| ty.to_string()).collect();
    format!("{key}[{}]", parts.join(","))
}

impl<'a> Compiler<'a> {
    fn ctx(&self) -> Ctx<'a> {
        Ctx {
            objs: self.objs,
            files: self.files,
            pkg: self.pkg.clone(),
            packages: self.packages.clone(),
        }
    }

    /// Switches to another package and type environment; returns the old ones.
    fn enter(&mut self, ctx: Ctx<'a>, env: HashMap<String, Ty>) -> (Ctx<'a>, HashMap<String, Ty>) {
        let old = self.ctx();
        self.objs = ctx.objs;
        self.files = ctx.files;
        self.pkg = ctx.pkg;
        self.packages = ctx.packages;
        (old, std::mem::replace(&mut self.type_env, env))
    }

    fn leave(&mut self, saved: (Ctx<'a>, HashMap<String, Ty>)) {
        self.enter(saved.0, saved.1);
    }

    pub(super) fn register_generic_type(&mut self, ts: &'a ast::TypeSpec) {
        let key = self.q(&self.objs.idents[ts.name].name);
        let ctx = self.ctx();
        self.generic_types.insert(
            key,
            GenericType {
                ts,
                ctx,
                methods: Vec::new(),
            },
        );
    }

    /// Records a generic function, or a method of a generic type. Returns
    /// false for an ordinary function.
    pub(super) fn register_generic_func(&mut self, fd: &'a ast::FuncDecl) -> bool {
        if let Some(type_key) = self.generic_receiver(fd) {
            if let Some(generic) = self.generic_types.get_mut(&type_key) {
                generic.methods.push(fd);
            }
            return true;
        }
        if self.objs.ftypes[fd.typ].typ_params.is_none() {
            return false;
        }
        let key = self.q(&self.objs.idents[fd.name].name);
        let ctx = self.ctx();
        self.generic_funcs.insert(key, GenericFunc { fd, ctx });
        true
    }

    /// Reports whether `fd` is generic: it is lowered per instance, not here.
    pub(super) fn is_generic_decl(&mut self, fd: &ast::FuncDecl) -> bool {
        self.objs.ftypes[fd.typ].typ_params.is_some() || self.generic_receiver(fd).is_some()
    }

    /// The generic type a method is declared on, as in `func (l *List[T]) Push`.
    fn generic_receiver(&mut self, fd: &ast::FuncDecl) -> Option<String> {
        let field_key = fd.recv.as_ref()?.list.first()?;
        let mut typ = strip_parens(&self.objs.fields[*field_key].typ);
        if let Expr::Star(star) = typ {
            typ = strip_parens(&star.expr);
        }
        let base = match typ {
            Expr::Index(ix) => &ix.expr,
            Expr::IndexList(ix) => &ix.expr,
            _ => return None,
        };
        let key = self.generic_key(base)?;
        self.generic_types.contains_key(&key).then_some(key)
    }

    /// The key that `e` would have as a package-level name: `Name` in the
    /// current package or `pkg.Name` in an imported one.
    pub(super) fn generic_key(&mut self, e: &Expr) -> Option<String> {
        match strip_parens(e) {
            Expr::Ident(key) => {
                let name = self.objs.idents[*key].name.as_str();
                if self.lookup(name).is_some() {
                    return None; // shadowed by a local
                }
                Some(self.q(name))
            }
            Expr::Selector(s) => {
                let package = self.package_of(&s.expr)?;
                Some(format!("{package}.{}", self.objs.idents[s.sel].name))
            }
            _ => None,
        }
    }

    /// The generic function `e` names, if any.
    pub(super) fn generic_func_key(&mut self, e: &Expr) -> Option<String> {
        let key = self.generic_key(e)?;
        self.generic_funcs.contains_key(&key).then_some(key)
    }

    /// `Name[args]` in a type position.
    pub(super) fn resolve_instance(&mut self, base: &Expr, args: &[Expr]) -> R<Ty> {
        let key = self.generic_key(base);
        let Some(key) = key.filter(|k| self.generic_types.contains_key(k)) else {
            return Err("type arguments on a type that is not generic".into());
        };
        let args = args
            .iter()
            .map(|arg| self.resolve_type(arg))
            .collect::<R<Vec<_>>>()?;
        self.instantiate_type(&key, args)
    }

    /// Returns the type for a generic type applied to `args`, creating it
    /// (and registering its methods) the first time.
    fn instantiate_type(&mut self, key: &str, args: Vec<Ty>) -> R<Ty> {
        let generic = &self.generic_types[key];
        let (ts, ctx, methods) = (generic.ts, generic.ctx.clone(), generic.methods.clone());
        let names = match &ts.typ_params {
            Some(list) => param_names(ctx.objs, list),
            None => Vec::new(),
        };
        if names.len() != args.len() {
            return Err(format!(
                "{key}: got {} type arguments but it has {} type parameters",
                args.len(),
                names.len()
            ));
        }
        let inst = instance_key(key, &args);
        if let Some(ty) = self.instances.get(&inst) {
            return Ok(ty.clone());
        }
        let env: HashMap<String, Ty> = names.into_iter().zip(args.iter().cloned()).collect();
        let saved = self.enter(ctx.clone(), env);
        let result = (|| {
            let ty = if let Expr::Struct(st) = &ts.typ {
                // Registered before its fields are resolved, so the type can
                // refer to itself, as in `next *Node[T]`.
                self.declare_struct_name(&inst);
                let ty = Ty::Struct(inst.clone());
                self.instances.insert(inst.clone(), ty.clone());
                self.define_struct(&inst, st)?;
                ty
            } else {
                let under = self.resolve_type(&ts.typ)?;
                let ty = Ty::Named(inst.clone(), Box::new(under));
                self.instances.insert(inst.clone(), ty.clone());
                ty
            };
            self.instance_args
                .insert(inst.clone(), (key.to_string(), args.clone()));

            for fd in methods {
                // A method names the type parameters itself, in its receiver.
                let recv_names = self.receiver_param_names(fd);
                let env: HashMap<String, Ty> =
                    recv_names.into_iter().zip(args.iter().cloned()).collect();
                let outer = std::mem::replace(&mut self.type_env, env.clone());
                let shape = self.func_shape(fd);
                self.type_env = outer;
                let shape = shape?;
                if let Some((type_name, method)) = shape.key.rsplit_once('.') {
                    let list = self.methods.entry(type_name.to_string()).or_default();
                    list.push(method.to_string());
                }
                self.register_instance(fd, shape, ctx.clone(), env);
            }
            Ok(ty)
        })();
        self.leave(saved);
        result
    }

    fn receiver_param_names(&self, fd: &ast::FuncDecl) -> Vec<String> {
        let objs = self.objs;
        let Some(field_key) = fd.recv.as_ref().and_then(|list| list.list.first()) else {
            return Vec::new();
        };
        let mut typ = strip_parens(&objs.fields[*field_key].typ);
        if let Expr::Star(star) = typ {
            typ = strip_parens(&star.expr);
        }
        let args: &[Expr] = match typ {
            Expr::Index(ix) => std::slice::from_ref(&ix.index),
            Expr::IndexList(ix) => &ix.indices,
            _ => &[],
        };
        args.iter()
            .map(|arg| match strip_parens(arg) {
                Expr::Ident(key) => objs.idents[*key].name.clone(),
                _ => "_".to_string(),
            })
            .collect()
    }

    /// Makes an instance callable and queues its body.
    fn register_instance(
        &mut self,
        fd: &'a ast::FuncDecl,
        shape: Shape,
        ctx: Ctx<'a>,
        env: HashMap<String, Ty>,
    ) {
        if self.funcs.contains_key(&shape.key) {
            return;
        }
        let symbol = self.symbol_for(&shape.key);
        self.pending_bodies.push(PendingBody {
            fd,
            key: shape.key.clone(),
            ctx,
            env,
        });
        self.funcs.insert(
            shape.key,
            FuncSig {
                symbol,
                params: shape.sig.params.into_iter().map(|(_, ty)| ty).collect(),
                ret: shape.sig.ret,
                ptr_recv: shape.ptr_recv,
                variadic: shape.sig.variadic,
                extern_c: false,
            },
        );
    }

    /// Returns the key in `funcs` of a generic function applied to `args`.
    pub(super) fn instantiate_func(&mut self, key: &str, args: Vec<Ty>) -> R<String> {
        let generic = &self.generic_funcs[key];
        let (fd, ctx) = (generic.fd, generic.ctx.clone());
        let names = match &ctx.objs.ftypes[fd.typ].typ_params {
            Some(list) => param_names(ctx.objs, list),
            None => Vec::new(),
        };
        if names.len() != args.len() {
            return Err(format!(
                "{key}: got {} type arguments but it has {} type parameters",
                args.len(),
                names.len()
            ));
        }
        let inst = instance_key(key, &args);
        if self.funcs.contains_key(&inst) {
            return Ok(inst);
        }
        let env: HashMap<String, Ty> = names.into_iter().zip(args).collect();
        let saved = self.enter(ctx.clone(), env.clone());
        let shape = self.func_shape(fd);
        self.leave(saved);
        let mut shape = shape?;
        shape.key = inst.clone();
        // The symbol belongs to the declaring package.
        let saved = self.enter(ctx.clone(), HashMap::new());
        self.register_instance(fd, shape, ctx, env);
        self.leave(saved);
        Ok(inst)
    }

    /// Lowers the bodies of the instances created so far, including the ones
    /// those bodies create in turn.
    pub(super) fn drain_instances(&mut self) -> R<()> {
        while let Some(pending) = self.pending_bodies.pop() {
            let saved = self.enter(pending.ctx, pending.env);
            let result = self.compile_func(pending.fd, &pending.key);
            self.leave(saved);
            result?;
        }
        Ok(())
    }

    /// A call to a generic function. `explicit` holds the type arguments
    /// written at the call; the rest are inferred from the arguments.
    pub(super) fn generic_call(
        &mut self,
        key: &str,
        explicit: Vec<Ty>,
        call: &ast::CallExpr,
        capture: bool,
    ) -> R<Val> {
        let generic = &self.generic_funcs[key];
        let (fd, objs) = (generic.fd, generic.ctx.objs);
        let ft = &objs.ftypes[fd.typ];
        let names = match &ft.typ_params {
            Some(list) => param_names(objs, list),
            None => Vec::new(),
        };
        if explicit.len() > names.len() {
            return Err(format!("{key}: too many type arguments"));
        }
        let mut env: HashMap<String, Ty> = names.iter().cloned().zip(explicit).collect();

        let mut vals = None;
        if env.len() < names.len() {
            // Infer the remaining type arguments from the call's arguments.
            let patterns = param_types(objs, &ft.params);
            let pattern_for = |i: usize| match patterns.get(i).or(patterns.last()) {
                Some(Expr::Ellipsis(e)) => (e.elt.as_ref(), true),
                other => (other.copied(), false),
            };
            let mut evaluated = Vec::new();
            for (i, arg) in call.args.iter().enumerate() {
                let val = self.expr(arg)?;
                let (pattern, variadic) = pattern_for(i);
                if let Some(pattern) = pattern
                    && !val.ty.is_untyped_num()
                    && val.ty != Ty::Nil
                {
                    // `f(xs...)` passes the whole slice for a `...T` parameter.
                    let actual = match val.ty.under() {
                        Ty::Slice(elem) if variadic && call.ellipsis.is_some() => elem.as_ref(),
                        _ => &val.ty,
                    };
                    self.unify(objs, &names, pattern, actual, &mut env);
                }
                evaluated.push(val);
            }
            // An untyped constant gives a parameter its default type, and a
            // floating-point constant wins over an integer one.
            let mut defaults: HashMap<String, Ty> = HashMap::new();
            for (i, val) in evaluated.iter().enumerate() {
                let (Some(Expr::Ident(ident)), _) = pattern_for(i) else {
                    continue;
                };
                let name = &objs.idents[*ident].name;
                if !val.ty.is_untyped_num() || !names.contains(name) || env.contains_key(name) {
                    continue;
                }
                let default = match val.ty {
                    Ty::UntypedFloat => Ty::Float(64),
                    Ty::UntypedRune => Ty::Int {
                        bits: 32,
                        signed: true,
                    },
                    _ => INT,
                };
                let keep_float = matches!(defaults.get(name), Some(Ty::Float(_)));
                if !keep_float {
                    defaults.insert(name.clone(), default);
                }
            }
            env.extend(defaults);
            if let Some(missing) = names.iter().find(|n| !env.contains_key(*n)) {
                return Err(format!(
                    "{key}: cannot infer the type argument {missing}; write it explicitly"
                ));
            }
            vals = Some(evaluated);
        }

        let args: Vec<Ty> = names.iter().map(|n| env[n].clone()).collect();
        let inst = self.instantiate_func(key, args)?;
        let sig = self.funcs[&inst].clone();
        // Arguments evaluated for inference are not evaluated again.
        self.pre_args = vals;
        self.user_call(&inst, &sig, None, call, capture)
    }

    /// Matches a parameter's type against an argument's type and binds the
    /// type parameters found. Mismatches are left for the later conversion
    /// of the argument to report.
    fn unify(
        &mut self,
        objs: &'a AstObjects,
        names: &[String],
        pattern: &Expr,
        actual: &Ty,
        env: &mut HashMap<String, Ty>,
    ) {
        match strip_parens(pattern) {
            Expr::Ident(key) => {
                let name = &objs.idents[*key].name;
                if names.contains(name) && !env.contains_key(name) {
                    env.insert(name.clone(), actual.clone());
                }
            }
            Expr::Star(star) => {
                if let Ty::Ptr(inner) = actual.under() {
                    self.unify(objs, names, &star.expr, inner, env);
                }
            }
            Expr::Array(array) => match actual.under() {
                Ty::Slice(elem) if array.len.is_none() => {
                    self.unify(objs, names, &array.elt, elem, env)
                }
                Ty::Array(elem, _) if array.len.is_some() => {
                    self.unify(objs, names, &array.elt, elem, env)
                }
                _ => {}
            },
            Expr::Map(map) => {
                if let Ty::Map(key, val) = actual.under() {
                    self.unify(objs, names, &map.key, key, env);
                    self.unify(objs, names, &map.val, val, env);
                }
            }
            Expr::Chan(chan) => {
                if let Ty::Chan(elem) = actual.under() {
                    self.unify(objs, names, &chan.val, elem, env);
                }
            }
            Expr::Func(key) => {
                let Ty::Func(sig) = actual.under() else {
                    return;
                };
                let ft = &objs.ftypes[*key];
                for (pattern, param) in param_types(objs, &ft.params).into_iter().zip(&sig.params) {
                    self.unify(objs, names, pattern, param, env);
                }
                let results = match &sig.ret {
                    Ty::Void => Vec::new(),
                    Ty::Tuple(items) => items.clone(),
                    other => vec![other.clone()],
                };
                if let Some(list) = &ft.results {
                    for (pattern, result) in param_types(objs, list).into_iter().zip(&results) {
                        self.unify(objs, names, pattern, result, env);
                    }
                }
            }
            // `List[T]` against an instance such as `List[int]`.
            Expr::Index(_) | Expr::IndexList(_) => {
                let patterns: &[Expr] = match strip_parens(pattern) {
                    Expr::Index(ix) => std::slice::from_ref(&ix.index),
                    Expr::IndexList(ix) => &ix.indices,
                    _ => &[],
                };
                let args = actual
                    .type_name()
                    .and_then(|name| self.instance_args.get(name))
                    .map(|(_, args)| args.clone());
                for (pattern, arg) in patterns.iter().zip(args.unwrap_or_default()) {
                    self.unify(objs, names, pattern, &arg, env);
                }
            }
            _ => {}
        }
    }
}
