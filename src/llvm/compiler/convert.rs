// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! Conversions between types: implicit (assignment) and explicit.

use super::*;

impl<'a> Compiler<'a> {
    /// Gives an untyped constant its default type.
    pub(super) fn materialize(&mut self, val: Val) -> R<Val> {
        match val.ty {
            Ty::UntypedInt => self.convert(val, &INT),
            Ty::UntypedRune => self.convert(
                val,
                &Ty::Int {
                    bits: 32,
                    signed: true,
                },
            ),
            Ty::UntypedFloat => self.convert(val, &Ty::Float(64)),
            Ty::Nil => Err("use of untyped nil".into()),
            Ty::Void => Err("expression has no value".into()),
            _ => Ok(val),
        }
    }

    /// Implicit conversion for assignment. Untyped constants and nil take the
    /// target type; a defined type and an unnamed composite type with the same
    /// underlying type are also assignable to each other.
    pub(super) fn coerce(&mut self, val: Val, to: &Ty) -> R<Val> {
        if val.ty == *to {
            return Ok(val);
        }
        if matches!(to.under(), Ty::Iface(_)) {
            return if val.ty == Ty::Nil {
                Ok(Val::new(to.clone(), "zeroinitializer"))
            } else {
                self.box_iface(val, to)
            };
        }
        if val.ty.is_untyped_num() {
            return self.convert(val, to);
        }
        match (&val.ty, to.under()) {
            (Ty::Nil, Ty::Ptr(_) | Ty::Map(..) | Ty::Chan(_)) => {
                return Ok(Val::new(to.clone(), "null"));
            }
            (Ty::Nil, Ty::Slice(_) | Ty::Func(_)) => {
                return Ok(Val::new(to.clone(), "zeroinitializer"));
            }
            _ => {}
        }
        let named = |ty: &Ty| matches!(ty, Ty::Named(..) | Ty::Struct(_));
        let composite = matches!(
            to.under(),
            Ty::Ptr(_)
                | Ty::Slice(_)
                | Ty::Array(..)
                | Ty::Tuple(_)
                | Ty::Func(_)
                | Ty::Map(..)
                | Ty::Chan(_)
        );
        if composite && val.ty.under() == to.under() && !(named(&val.ty) && named(to)) {
            return Ok(Val {
                ty: to.clone(),
                ..val
            });
        }
        Err(format!("cannot use {} as {to}", val.ty))
    }

    /// Explicit conversion `T(x)`.
    pub(super) fn convert(&mut self, val: Val, to: &Ty) -> R<Val> {
        if val.ty == *to {
            return Ok(val);
        }
        if matches!(to.under(), Ty::Iface(_)) || val.ty == Ty::Nil {
            return self.coerce(val, to);
        }
        let byte = Ty::Int {
            bits: 8,
            signed: false,
        };
        if val.ty.is_untyped_num() {
            let k = val.k.ok_or("internal: untyped value without a constant")?;
            return match (to.under(), k) {
                (Ty::Int { bits, signed }, K::Int(v)) => {
                    let (min, max) = if *signed {
                        (-(1i128 << (bits - 1)), (1i128 << (bits - 1)) - 1)
                    } else {
                        (0, (1i128 << bits) - 1)
                    };
                    if v < min || v > max {
                        return Err(format!("constant {v} overflows {to}"));
                    }
                    Ok(Val::int(to.clone(), v))
                }
                (Ty::Int { .. }, K::Float(v)) => {
                    if v.fract() != 0.0 {
                        return Err(format!("constant {v} truncated to integer"));
                    }
                    self.convert(Val::int(Ty::UntypedInt, v as i128), to)
                }
                (Ty::Float(bits), k) => Ok(Val {
                    ty: to.clone(),
                    ..Val::float(Ty::Float(*bits), k.as_f64())
                }),
                (Ty::Str, K::Int(_)) => {
                    let rune = self.materialize(Val {
                        ty: Ty::UntypedRune,
                        ..val
                    })?;
                    self.convert(rune, to)
                }
                _ => Err(format!("cannot convert {} constant to {to}", val.ty)),
            };
        }
        // Identical underlying types share a representation.
        if val.ty.under() == to.under() {
            return Ok(Val {
                ty: to.clone(),
                ..val
            });
        }
        let (from_ll, to_ll) = (val.ty.ll(), to.ll());
        let cast = match (val.ty.under(), to.under()) {
            (Ty::Int { bits: from, signed }, Ty::Int { bits: into, .. }) => {
                if from == into {
                    return Ok(Val {
                        ty: to.clone(),
                        ..val
                    });
                }
                if from > into {
                    "trunc"
                } else if *signed {
                    "sext"
                } else {
                    "zext"
                }
            }
            (Ty::Int { signed, .. }, Ty::Float(_)) => {
                if *signed {
                    "sitofp"
                } else {
                    "uitofp"
                }
            }
            (Ty::Float(_), Ty::Int { signed, .. }) => {
                if *signed {
                    "fptosi"
                } else {
                    "fptoui"
                }
            }
            (Ty::Float(from), Ty::Float(into)) => {
                if from < into {
                    "fpext"
                } else {
                    "fptrunc"
                }
            }
            (Ty::Int { .. }, Ty::Str) => {
                // string(rune): the UTF-8 encoding of one code point.
                let rune = self.convert(
                    val,
                    &Ty::Int {
                        bits: 32,
                        signed: true,
                    },
                )?;
                let buf = self.alloca(&Ty::Array(Box::new(byte), 4));
                let len = self.ins(format!(
                    "call i64 @goiaba_encode_rune(i32 {}, ptr {buf})",
                    rune.repr
                ));
                let data = self.ins(format!("call ptr @goiaba.clone(ptr {buf}, i64 {len})"));
                let s = self.make_string(&data, &len);
                return Ok(Val::new(to.clone(), s.repr));
            }
            (Ty::Slice(elem), Ty::Str) if **elem == byte => {
                let data = self.ins(format!("extractvalue %slice {}, 0", val.repr));
                let len = self.ins(format!("extractvalue %slice {}, 1", val.repr));
                let copy = self.ins(format!("call ptr @goiaba.clone(ptr {data}, i64 {len})"));
                let s = self.make_string(&copy, &len);
                return Ok(Val::new(to.clone(), s.repr));
            }
            (Ty::Str, Ty::Slice(elem)) if **elem == byte => {
                let data = self.ins(format!("extractvalue %string {}, 0", val.repr));
                let len = self.ins(format!("extractvalue %string {}, 1", val.repr));
                let copy = self.ins(format!("call ptr @goiaba.clone(ptr {data}, i64 {len})"));
                return Ok(self.make_slice(to.clone(), &copy, &len, &len));
            }
            _ => return Err(format!("cannot convert {} to {to}", val.ty)),
        };
        let name = self.ins(format!("{cast} {from_ll} {} to {to_ll}", val.repr));
        Ok(Val::new(to.clone(), name))
    }

    /// Widens an integer operand to i64, for indexes and lengths.
    pub(super) fn widen_i64(&mut self, val: Val) -> R<String> {
        let val = self.materialize(val)?;
        if !matches!(val.ty.under(), Ty::Int { .. }) {
            return Err(format!("expected an integer, found {}", val.ty));
        }
        Ok(self.convert(val, &INT)?.repr)
    }
}
