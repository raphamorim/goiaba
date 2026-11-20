// Copyright 2025-present Raphael Amorim. All rights reserved.
// Use of this source code is governed by a BSD-3-Clause
// license that can be found in the LICENSE file.

//! Literals and constant folding.

use super::*;

pub(super) fn parse_int(lit: &str) -> R<i128> {
    let clean = lit.replace('_', "");
    let lower = clean.to_ascii_lowercase();
    let (radix, digits) = if let Some(rest) = lower.strip_prefix("0x") {
        (16, rest)
    } else if let Some(rest) = lower.strip_prefix("0b") {
        (2, rest)
    } else if let Some(rest) = lower.strip_prefix("0o") {
        (8, rest)
    } else if lower.len() > 1 && lower.starts_with('0') {
        (8, &lower[1..])
    } else {
        (10, lower.as_str())
    };
    i128::from_str_radix(digits, radix).map_err(|_| format!("invalid integer literal {lit}"))
}

/// Decodes the escapes of an interpreted string or rune literal body.
pub(super) fn unescape(body: &str) -> R<Vec<u8>> {
    let mut out = Vec::new();
    let mut chars = body.chars();
    let hex = |chars: &mut std::str::Chars<'_>, n: usize| -> R<u32> {
        let digits: String = chars.by_ref().take(n).collect();
        if digits.len() != n {
            return Err(format!("invalid escape in literal {body}"));
        }
        u32::from_str_radix(&digits, 16).map_err(|_| format!("invalid escape in literal {body}"))
    };
    while let Some(c) = chars.next() {
        if c != '\\' {
            let mut buf = [0u8; 4];
            out.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
            continue;
        }
        let Some(e) = chars.next() else {
            return Err(format!("invalid escape in literal {body}"));
        };
        match e {
            'a' => out.push(7),
            'b' => out.push(8),
            'f' => out.push(12),
            'n' => out.push(b'\n'),
            'r' => out.push(b'\r'),
            't' => out.push(b'\t'),
            'v' => out.push(11),
            '\\' => out.push(b'\\'),
            '\'' => out.push(b'\''),
            '"' => out.push(b'"'),
            'x' => out.push(hex(&mut chars, 2)? as u8),
            'u' | 'U' => {
                let code = hex(&mut chars, if e == 'u' { 4 } else { 8 })?;
                let c = char::from_u32(code)
                    .ok_or_else(|| format!("invalid unicode escape in literal {body}"))?;
                let mut buf = [0u8; 4];
                out.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
            }
            '0'..='7' => {
                let mut value = e.to_digit(8).unwrap();
                for _ in 0..2 {
                    let d = chars
                        .next()
                        .and_then(|c| c.to_digit(8))
                        .ok_or_else(|| format!("invalid octal escape in literal {body}"))?;
                    value = value * 8 + d;
                }
                out.push(value as u8);
            }
            _ => return Err(format!("unknown escape \\{e} in literal {body}")),
        }
    }
    Ok(out)
}

pub(super) fn unquote_string(lit: &str) -> R<Vec<u8>> {
    if let Some(raw) = lit.strip_prefix('`') {
        let raw = raw.strip_suffix('`').unwrap_or(raw);
        return Ok(raw.replace('\r', "").into_bytes());
    }
    let body = lit.strip_prefix('"').unwrap_or(lit);
    let body = body.strip_suffix('"').unwrap_or(body);
    unescape(body)
}

pub(super) fn unquote_rune(lit: &str) -> R<i128> {
    let body = lit.strip_prefix('\'').unwrap_or(lit);
    let body = body.strip_suffix('\'').unwrap_or(body);
    let bytes = unescape(body)?;
    let byte_escape = body.starts_with("\\x")
        || (body.starts_with('\\') && body[1..].starts_with(|c: char| c.is_digit(8)));
    if byte_escape && bytes.len() == 1 {
        return Ok(bytes[0] as i128);
    }
    let text = String::from_utf8(bytes).map_err(|_| format!("invalid rune literal {lit}"))?;
    let mut chars = text.chars();
    match (chars.next(), chars.next()) {
        (Some(c), None) => Ok(c as i128),
        _ => Err(format!("invalid rune literal {lit}")),
    }
}

/// Constant-folds an operation on two untyped numeric constants.
pub(super) fn fold(op: Op, left: &Val, right: &Val) -> R<Val> {
    let ty = if left.ty == Ty::UntypedFloat || right.ty == Ty::UntypedFloat {
        Ty::UntypedFloat
    } else if left.ty == Ty::UntypedRune || right.ty == Ty::UntypedRune {
        Ty::UntypedRune
    } else {
        Ty::UntypedInt
    };
    let overflow = || "constant overflow".to_string();
    match (left.k, right.k) {
        (Some(K::Int(a)), Some(K::Int(b))) => {
            let v = match op {
                Op::Add => a.checked_add(b).ok_or_else(overflow)?,
                Op::Sub => a.checked_sub(b).ok_or_else(overflow)?,
                Op::Mul => a.checked_mul(b).ok_or_else(overflow)?,
                Op::Div | Op::Rem if b == 0 => return Err("division by zero".into()),
                Op::Div => a / b,
                Op::Rem => a % b,
                Op::And => a & b,
                Op::Or => a | b,
                Op::Xor => a ^ b,
                Op::AndNot => a & !b,
                Op::Eq => return Ok(Val::bool(a == b)),
                Op::Ne => return Ok(Val::bool(a != b)),
                Op::Lt => return Ok(Val::bool(a < b)),
                Op::Le => return Ok(Val::bool(a <= b)),
                Op::Gt => return Ok(Val::bool(a > b)),
                Op::Ge => return Ok(Val::bool(a >= b)),
                Op::Shl | Op::Shr => return Err("internal: shift reached fold".into()),
            };
            Ok(Val::int(ty, v))
        }
        (Some(a), Some(b)) => {
            let (a, b) = (a.as_f64(), b.as_f64());
            let v = match op {
                Op::Add => a + b,
                Op::Sub => a - b,
                Op::Mul => a * b,
                Op::Div if b == 0.0 => return Err("division by zero".into()),
                Op::Div => a / b,
                Op::Eq => return Ok(Val::bool(a == b)),
                Op::Ne => return Ok(Val::bool(a != b)),
                Op::Lt => return Ok(Val::bool(a < b)),
                Op::Le => return Ok(Val::bool(a <= b)),
                Op::Gt => return Ok(Val::bool(a > b)),
                Op::Ge => return Ok(Val::bool(a >= b)),
                _ => {
                    return Err(format!(
                        "operator {} is not defined on untyped float",
                        op.symbol()
                    ));
                }
            };
            Ok(Val::float(Ty::UntypedFloat, v))
        }
        _ => Err("internal: untyped value without a constant".into()),
    }
}
