// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! Lowering of function bodies: frames for defer, closures, the instruction emitter and scopes.

use super::*;

impl<'a> Compiler<'a> {
    /// Lowers one function body and returns its definition. `self.f` is left
    /// holding the final state, so the caller can read captures and closures.
    pub(super) fn lower_function(
        &mut self,
        symbol: &str,
        sig: &Signature,
        body: &[Stmt],
        exported: bool,
        closure: bool,
    ) -> R<String> {
        let result_types = match &sig.ret {
            Ty::Tuple(items) => items.clone(),
            Ty::Void => Vec::new(),
            other => vec![other.clone()],
        };

        // Lower again while the set of address-taken locals grows, or when a
        // defer statement shows that the function needs a defer frame.
        let mut escaping = HashSet::new();
        let mut uses_defer = false;
        loop {
            if uses_defer {
                // Deferred calls and recovery read named results from memory.
                escaping.extend(sig.results.iter().cloned());
            }
            self.f = FnCtx::new(sig.ret.clone(), escaping);
            self.f.symbol = symbol.to_string();
            self.f.uses_defer = uses_defer;
            self.f.scopes.push(HashMap::new());
            for (i, (pname, ty)) in sig.params.iter().enumerate() {
                if pname != "_" {
                    let arg = Val::new(ty.clone(), format!("%arg{i}"));
                    self.declare(pname, ty.clone(), Some(arg));
                }
            }
            for (rname, ty) in sig.results.iter().zip(&result_types) {
                self.declare(rname, ty.clone(), None);
            }
            self.f.named_results = sig.results.clone();
            if uses_defer {
                self.defer_prologue(&result_types);
            }
            self.block(body)?;
            if uses_defer {
                if !self.f.terminated && sig.ret != Ty::Void {
                    self.emit("unreachable".into());
                    self.f.terminated = true;
                }
                self.defer_epilogue(&sig.ret);
            } else if !self.f.terminated {
                if sig.ret == Ty::Void {
                    self.emit("ret void".into());
                } else {
                    self.emit("unreachable".into());
                }
            }
            if !self.f.retry {
                break;
            }
            uses_defer |= self.f.wants_defer;
            escaping = std::mem::take(&mut self.f.escaping);
        }

        let mut params: Vec<String> = sig
            .params
            .iter()
            .enumerate()
            .map(|(i, (_, ty))| format!("{} %arg{i}", ty.ll()))
            .collect();
        if closure {
            params.insert(0, "ptr %ctx".to_string());
        }
        let linkage = if exported { "" } else { "internal " };
        let mut def = format!(
            "define {linkage}{} @\"{symbol}\"({}) {{\nentry:\n",
            sig.ret.ll(),
            params.join(", ")
        );
        // A closure reads the addresses of its captured variables from its context.
        for i in 0..self.f.captures.len() {
            let _ = writeln!(
                def,
                "  %cap{i}.slot = getelementptr inbounds ptr, ptr %ctx, i64 {i}\n  %cap{i} = load ptr, ptr %cap{i}.slot"
            );
        }
        for line in self.f.allocas.iter().chain(&self.f.body) {
            def.push_str(line);
            def.push('\n');
        }
        def.push_str("}\n");
        Ok(def)
    }

    /// Sets up the defer frame. `_setjmp` returns a second time, with a
    /// non-zero value, when a deferred call recovers from a panic; the
    /// function then leaves through `fn.exit` like a normal return.
    pub(super) fn defer_prologue(&mut self, result_types: &[Ty]) {
        self.f.allocas.push("  %frame = alloca [3 x ptr]".into());
        self.f
            .allocas
            .push("  %env = alloca [512 x i8], align 16".into());
        let names = self.f.named_results.clone();
        for (i, ty) in result_types.iter().enumerate() {
            let slot = match names.get(i).and_then(|name| self.lookup(name)) {
                Some(Binding::Var { ptr, .. }) => ptr,
                _ => {
                    let slot = self.alloca(ty);
                    self.store(&Val::new(ty.clone(), "zeroinitializer"), &slot);
                    slot
                }
            };
            self.f.ret_slots.push((slot, ty.clone()));
        }
        self.emit("call void @goiaba_frame_enter(ptr %frame, ptr %env)".into());
        let again = self.ins("call i32 @_setjmp(ptr %env) returns_twice".into());
        let recovered = self.ins(format!("icmp ne i32 {again}, 0"));
        self.cond_br(&recovered, "fn.exit", "fn.body");
        self.place_label("fn.body");
    }

    /// Runs the deferred calls and returns the stored results.
    pub(super) fn defer_epilogue(&mut self, ret: &Ty) {
        if !self.f.terminated {
            self.br("fn.exit");
        }
        self.place_label("fn.exit");
        self.emit("call void @goiaba_frame_exit(ptr %frame)".into());
        let slots = self.f.ret_slots.clone();
        // Volatile, so the values survive the jump back from a recovered panic.
        let vals: Vec<Val> = slots
            .iter()
            .map(|(slot, ty)| {
                let name = self.ins(format!("load volatile {}, ptr {slot}", ty.ll()));
                Val::new(ty.clone(), name)
            })
            .collect();
        self.emit_ret(ret, &vals);
    }

    pub(super) fn emit_ret(&mut self, ret: &Ty, vals: &[Val]) {
        match vals {
            [] => self.emit("ret void".into()),
            [val] => self.emit(format!("ret {} {}", ret.ll(), val.repr)),
            _ => {
                let ll = ret.ll();
                let mut cur = "poison".to_string();
                for (i, val) in vals.iter().enumerate() {
                    cur = self.ins(format!(
                        "insertvalue {ll} {cur}, {} {}, {i}",
                        val.ty.ll(),
                        val.repr
                    ));
                }
                self.emit(format!("ret {ll} {cur}"));
            }
        }
        self.f.terminated = true;
    }

    /// `defer f(args)`: the callee and arguments are evaluated now and saved
    /// in a record; a small thunk performs the call when the function exits.
    pub(super) fn defer_stmt(&mut self, d: &ast::DeferStmt) -> R<()> {
        let Expr::Call(call) = strip_parens(&d.call) else {
            return Err("defer needs a function call".into());
        };
        if !self.f.uses_defer {
            self.f.wants_defer = true;
            self.f.retry = true;
            return Ok(());
        }
        let record = self.call_record(call, "defer")?;
        self.emit(format!(
            "call void @goiaba_defer_push(ptr %frame, ptr {record})"
        ));
        Ok(())
    }

    /// Evaluates a call without performing it and returns a heap record
    /// `{ next, thunk, operands... }` whose thunk makes the call.
    pub(super) fn call_record(&mut self, call: &ast::CallExpr, what: &str) -> R<String> {
        self.f.pending = None;
        self.call_mode(call, true)?;
        let Some(pending) = self.f.pending.take() else {
            return Err(format!(
                "{what} of a builtin is not supported yet; wrap it in a function literal"
            ));
        };
        let fields: String = pending
            .args
            .iter()
            .map(|(ty, _)| format!(", {ty}"))
            .collect();
        let record_ty = format!("{{ ptr, ptr{fields} }}");
        self.f.closure_count += 1;
        let thunk = format!("{}$call{}", self.f.symbol, self.f.closure_count);

        let mut def = format!("define internal void @\"{thunk}\"(ptr %rec) {{\nentry:\n");
        let mut operands = Vec::new();
        for (i, (ty, _)) in pending.args.iter().enumerate() {
            let _ = writeln!(
                def,
                "  %p{i} = getelementptr inbounds {record_ty}, ptr %rec, i32 0, i32 {}\n  %v{i} = load {ty}, ptr %p{i}",
                i + 2
            );
            operands.push(format!("{ty} %v{i}"));
        }
        let callee = match &pending.symbol {
            Some(symbol) => symbol.clone(),
            None => {
                operands.remove(0);
                "%v0".to_string()
            }
        };
        let ret = pending.ret.ll();
        let result = if pending.ret == Ty::Void { "" } else { "%r = " };
        let _ = write!(
            def,
            "  {result}call {ret} {callee}({})\n  ret void\n}}\n",
            operands.join(", ")
        );
        self.f.nested_defs.push(def);

        let record = self.ins(format!(
            "call ptr @goiaba_alloc(i64 ptrtoint (ptr getelementptr ({record_ty}, ptr null, i32 1) to i64))"
        ));
        let slot = self.ins(format!(
            "getelementptr inbounds {record_ty}, ptr {record}, i32 0, i32 1"
        ));
        self.emit(format!("store ptr @\"{thunk}\", ptr {slot}"));
        for (i, (ty, value)) in pending.args.iter().enumerate() {
            let slot = self.ins(format!(
                "getelementptr inbounds {record_ty}, ptr {record}, i32 0, i32 {}",
                i + 2
            ));
            self.emit(format!("store {ty} {value}, ptr {slot}"));
        }
        Ok(record)
    }

    /// Emits a call, or saves it in `self.f.pending` when `capture` is set.
    /// `symbol` is `None` when the code pointer is the first of `args`.
    pub(super) fn finish_call(
        &mut self,
        ret: &Ty,
        symbol: Option<String>,
        args: Vec<(String, String)>,
        capture: bool,
    ) -> Val {
        if capture {
            self.f.pending = Some(Pending {
                ret: ret.clone(),
                symbol,
                args,
            });
            return Val::void();
        }
        let mut operands: Vec<String> = args.iter().map(|(ty, v)| format!("{ty} {v}")).collect();
        let callee = match symbol {
            Some(symbol) => symbol,
            None => {
                operands.remove(0);
                args[0].1.clone()
            }
        };
        let target = format!("{} {callee}({})", ret.ll(), operands.join(", "));
        if *ret == Ty::Void {
            self.emit(format!("call {target}"));
            Val::void()
        } else {
            let result = self.ins(format!("call {target}"));
            Val::new(ret.clone(), result)
        }
    }

    /// Lowers a function literal to a closure value.
    pub(super) fn func_lit(&mut self, lit: &ast::FuncLit) -> R<Val> {
        let sig = self.signature(&self.objs.ftypes[lit.typ])?;
        self.f.closure_count += 1;
        let symbol = format!("{}$f{}", self.f.symbol, self.f.closure_count);
        let ty = Ty::Func(Box::new(sig.func_ty()));

        let parent = std::mem::replace(&mut self.f, FnCtx::new(Ty::Void, HashSet::new()));
        self.outer.push(parent);
        let result = self.lower_function(&symbol, &sig, &lit.body.list, false, true);
        let parent = self.outer.pop().expect("closure has an enclosing function");
        let child = std::mem::replace(&mut self.f, parent);
        let def = result?;
        self.f.nested_defs.extend(child.nested_defs);
        self.f.nested_defs.push(def);

        if child.captures.is_empty() {
            return Ok(Val::new(ty, format!("{{ ptr @\"{symbol}\", ptr null }}")));
        }
        let count = child.captures.len().to_string();
        let ctx = self.alloc_array(&count, &Ty::Nil);
        for (i, cap) in child.captures.iter().enumerate() {
            let ptr = if cap.indirect {
                self.ins(format!("load ptr, ptr {}", cap.ptr))
            } else {
                cap.ptr.clone()
            };
            let slot = self.ins(format!("getelementptr inbounds ptr, ptr {ctx}, i64 {i}"));
            self.emit(format!("store ptr {ptr}, ptr {slot}"));
        }
        let a = self.ins(format!("insertvalue %func poison, ptr @\"{symbol}\", 0"));
        let b = self.ins(format!("insertvalue %func {a}, ptr {ctx}, 1"));
        Ok(Val::new(ty, b))
    }

    /// A top-level function used as a value. Function values are called with
    /// a leading context pointer, so the function gets a thunk that drops it.
    pub(super) fn func_value(&mut self, name: &str) -> R<Val> {
        let sig = self.funcs[name].clone();
        let thunk = format!("{}$thunk", sig.symbol);
        if self.thunks.insert(thunk.clone()) {
            let mut params = vec!["ptr %ctx".to_string()];
            let mut args = Vec::new();
            for (i, ty) in sig.params.iter().enumerate() {
                params.push(format!("{} %arg{i}", ty.ll()));
                args.push(format!("{} %arg{i}", ty.ll()));
            }
            let ret = sig.ret.ll();
            let call = format!("call {ret} @\"{}\"({})", sig.symbol, args.join(", "));
            let body = if sig.ret == Ty::Void {
                format!("  {call}\n  ret void")
            } else {
                format!("  %r = {call}\n  ret {ret} %r")
            };
            self.fn_defs.push(format!(
                "define internal {ret} @\"{thunk}\"({}) {{\nentry:\n{body}\n}}\n",
                params.join(", ")
            ));
        }
        let ty = Ty::Func(Box::new(FuncTy {
            params: sig.params.clone(),
            ret: sig.ret.clone(),
            variadic: sig.variadic,
        }));
        Ok(Val::new(ty, format!("{{ ptr @\"{thunk}\", ptr null }}")))
    }

    pub(super) fn fresh(&mut self) -> usize {
        self.f.counter += 1;
        self.f.counter
    }

    pub(super) fn new_label(&mut self, hint: &str) -> String {
        let n = self.fresh();
        format!("{hint}.{n}")
    }

    pub(super) fn emit(&mut self, line: String) {
        if self.f.terminated {
            // Code after a terminator is unreachable; give it its own block.
            let label = self.new_label("dead");
            self.f.body.push(format!("{label}:"));
            self.f.cur = label;
            self.f.terminated = false;
        }
        self.f.body.push(format!("  {line}"));
    }

    /// Emits `%tN = <rhs>` and returns the new name.
    pub(super) fn ins(&mut self, rhs: String) -> String {
        let name = format!("%t{}", self.fresh());
        self.emit(format!("{name} = {rhs}"));
        name
    }

    pub(super) fn br(&mut self, label: &str) {
        if !self.f.terminated {
            self.emit(format!("br label %{label}"));
            self.f.terminated = true;
        }
    }

    pub(super) fn cond_br(&mut self, cond: &str, then: &str, els: &str) {
        self.emit(format!("br i1 {cond}, label %{then}, label %{els}"));
        self.f.terminated = true;
    }

    pub(super) fn place_label(&mut self, label: &str) {
        if !self.f.terminated {
            self.f.body.push(format!("  br label %{label}"));
        }
        self.f.body.push(format!("{label}:"));
        self.f.cur = label.to_string();
        self.f.terminated = false;
    }

    pub(super) fn alloca(&mut self, ty: &Ty) -> String {
        let name = format!("%a{}", self.fresh());
        self.f
            .allocas
            .push(format!("  {name} = alloca {}", ty.ll()));
        name
    }

    pub(super) fn heap_alloc(&mut self, ty: &Ty) -> String {
        self.ins(format!("call ptr @goiaba_alloc(i64 {})", sizeof(ty)))
    }

    /// Allocates zeroed room for `count` values of `elem`.
    pub(super) fn alloc_array(&mut self, count: &str, elem: &Ty) -> String {
        let bytes = self.ins(format!("mul i64 {count}, {}", sizeof(elem)));
        self.ins(format!("call ptr @goiaba_alloc(i64 {bytes})"))
    }

    pub(super) fn nilcheck(&mut self, ptr: &str) {
        self.emit(format!("call void @goiaba.nilcheck(ptr {ptr})"));
    }

    pub(super) fn make_string(&mut self, data: &str, len: &str) -> Val {
        let a = self.ins(format!("insertvalue %string poison, ptr {data}, 0"));
        let b = self.ins(format!("insertvalue %string {a}, i64 {len}, 1"));
        Val::new(Ty::Str, b)
    }

    pub(super) fn store(&mut self, val: &Val, ptr: &str) {
        self.emit(format!("store {} {}, ptr {ptr}", val.ty.ll(), val.repr));
    }

    pub(super) fn load(&mut self, ptr: &str, ty: &Ty) -> Val {
        let name = self.ins(format!("load {}, ptr {ptr}", ty.ll()));
        Val::new(ty.clone(), name)
    }

    /// Copies a value into a stack slot and returns the slot.
    pub(super) fn spill(&mut self, val: &Val) -> String {
        let slot = self.alloca(&val.ty);
        self.store(val, &slot);
        slot
    }

    pub(super) fn intern(&mut self, bytes: Vec<u8>) -> String {
        let id = match self.string_ids.get(&bytes) {
            Some(id) => *id,
            None => {
                let id = self.strings.len();
                self.strings.push(bytes.clone());
                self.string_ids.insert(bytes, id);
                id
            }
        };
        format!("@.str.{id}")
    }

    /// A NUL-terminated constant for libc calls.
    pub(super) fn cstr(&mut self, text: &str) -> String {
        let mut bytes = text.as_bytes().to_vec();
        bytes.push(0);
        self.intern(bytes)
    }

    pub(super) fn string_const(&mut self, bytes: Vec<u8>) -> Val {
        if bytes.is_empty() {
            return Val::new(Ty::Str, "zeroinitializer");
        }
        let len = bytes.len();
        let global = self.intern(bytes);
        Val::new(Ty::Str, format!("{{ ptr {global}, i64 {len} }}"))
    }

    pub(super) fn panic_if(&mut self, cond: &str, message: &str) {
        let msg = self.cstr(message);
        self.emit(format!("call void @goiaba.panic_if(i1 {cond}, ptr {msg})"));
    }

    pub(super) fn push_scope(&mut self) {
        self.f.scopes.push(HashMap::new());
    }

    pub(super) fn pop_scope(&mut self) {
        self.f.scopes.pop();
    }

    pub(super) fn lookup(&mut self, name: &str) -> Option<Binding> {
        let local = self.f.scopes.iter().rev().find_map(|scope| scope.get(name));
        if let Some(binding) = local {
            return Some(binding.clone());
        }
        if let Some(binding) = self.capture(name, self.outer.len()) {
            return Some(binding);
        }
        self.globals.get(&self.q(name)).cloned()
    }

    /// Resolves `name` for the function at nesting `level` by looking in its
    /// enclosing functions, and records it as a capture. Level 0 is the
    /// outermost function; `self.outer.len()` is the one being lowered.
    pub(super) fn capture(&mut self, name: &str, level: usize) -> Option<Binding> {
        if level == 0 {
            return None;
        }
        let parent = &self.outer[level - 1];
        let found = parent
            .scopes
            .iter()
            .rev()
            .find_map(|s| s.get(name))
            .cloned();
        let found = match found {
            Some(binding) => binding,
            None => self.capture(name, level - 1)?,
        };
        let Binding::Var {
            ptr,
            ty,
            heap,
            indirect,
        } = found
        else {
            return Some(found);
        };
        if !heap {
            // A captured variable must outlive its frame.
            let parent = &mut self.outer[level - 1];
            if parent.escaping.insert(name.to_string()) {
                parent.retry = true;
            }
        }
        let ctx = if level == self.outer.len() {
            &mut self.f
        } else {
            &mut self.outer[level]
        };
        let local = format!("%cap{}", ctx.captures.len());
        ctx.captures.push(Capture { ptr, indirect });
        let binding = Binding::Var {
            ptr: local,
            ty,
            heap: true,
            indirect: false,
        };
        ctx.scopes[0].insert(name.to_string(), binding.clone());
        Some(binding)
    }

    pub(super) fn bind(&mut self, name: &str, binding: Binding) {
        if name == "_" {
            return;
        }
        match self.f.scopes.last_mut() {
            Some(scope) => {
                scope.insert(name.to_string(), binding);
            }
            None => {
                let key = self.q(name);
                self.globals.insert(key, binding);
            }
        }
    }

    /// Declares a local. Without `init` the variable holds its zero value.
    pub(super) fn declare(&mut self, name: &str, ty: Ty, init: Option<Val>) {
        let heap = self.f.escaping.contains(name);
        let ptr = if heap {
            self.heap_alloc(&ty)
        } else {
            self.alloca(&ty)
        };
        match init {
            Some(val) => self.store(&val, &ptr),
            None if !heap => self.store(&Val::new(ty.clone(), "zeroinitializer"), &ptr),
            None => {}
        }
        self.bind(
            name,
            Binding::Var {
                ptr,
                ty,
                heap,
                indirect: false,
            },
        );
    }

    /// Records that the variable at the root of `e` has its address taken.
    pub(super) fn mark_escape(&mut self, e: &Expr) {
        let objs = self.objs;
        let mut e = e;
        let name = loop {
            match e {
                Expr::Paren(p) => e = &p.expr,
                Expr::Selector(s) => e = &s.expr,
                Expr::Index(ix) => e = &ix.expr,
                Expr::Ident(key) => break objs.idents[*key].name.as_str(),
                _ => return,
            }
        };
        if let Some(Binding::Var { heap: false, .. }) = self.lookup(name)
            && self.f.escaping.insert(name.to_string())
        {
            self.f.retry = true;
        }
    }
}
