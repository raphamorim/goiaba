package fmt

import (
	"io"
	"os"
	"strconv"
)

// Value kinds, as reported by the runtime.
const (
	kindNil       = 0
	kindBool      = 1
	kindInt       = 2
	kindUint      = 3
	kindFloat     = 4
	kindString    = 5
	kindPointer   = 6
	kindSlice     = 9
	kindArray     = 10
	kindStruct    = 11
	kindMap       = 12
	kindInterface = 13
	kindFunc      = 14
)

func kind(v any) int
func size(v any) int
func intOf(v any) int64
func uintOf(v any) uint64
func floatOf(v any) float64
func boolOf(v any) bool
func stringOf(v any, out *string)
func typeName(v any, out *string)
func pointerOf(v any) uint64
func length(v any) int
func elem(v any, i int, out *any)
func fieldName(v any, i int, out *string)
func elemKind(v any) int
func mapNext(v any, cursor *int, key *any, val *any) bool

// Stringer is implemented by any value that has a String method.
type Stringer interface {
	String() string
}

// printer accumulates formatted output.
type printer struct {
	buf []byte

	// Flags of the verb being processed.
	minus, plus, sharp, space, zero bool
	// plusV is the + flag of %+v: print struct field names.
	plusV bool
	width, prec                   int
	hasWidth, hasPrec             bool
}

func (p *printer) writeString(s string) { p.buf = append(p.buf, s...) }

// pad writes s padded to the current width.
func (p *printer) pad(s string) {
	n := 0
	for range s {
		n++
	}
	if !p.hasWidth || n >= p.width {
		p.writeString(s)
		return
	}
	fill := p.width - n
	if p.minus {
		p.writeString(s)
		for i := 0; i < fill; i++ {
			p.buf = append(p.buf, ' ')
		}
		return
	}
	c := byte(' ')
	if p.zero {
		c = '0'
		// Zeros go after the sign.
		if len(s) > 0 && (s[0] == '-' || s[0] == '+') {
			p.buf = append(p.buf, s[0])
			s = s[1:]
		}
	}
	for i := 0; i < fill; i++ {
		p.buf = append(p.buf, c)
	}
	p.writeString(s)
}

func (p *printer) badVerb(verb rune, v any) {
	p.writeString("%!")
	p.writeString(string(verb))
	p.writeString("(")
	if v == nil {
		p.writeString("<nil>")
	} else {
		var name string
		typeName(v, &name)
		p.writeString(name)
		p.writeString("=")
		p.value(v, 'v', 0)
	}
	p.writeString(")")
}

func (p *printer) fmtInt(neg bool, u uint64, verb rune) {
	var s string
	switch verb {
	case 'd', 'v':
		s = strconv.FormatUint(u, 10)
	case 'b':
		s = strconv.FormatUint(u, 2)
	case 'o':
		s = strconv.FormatUint(u, 8)
	case 'x':
		s = strconv.FormatUint(u, 16)
		if p.sharp {
			s = "0x" + s
		}
	case 'X':
		s = upper(strconv.FormatUint(u, 16))
		if p.sharp {
			s = "0X" + s
		}
	case 'c':
		p.pad(string(rune(u)))
		return
	case 'q':
		p.pad("'" + string(rune(u)) + "'")
		return
	}
	if p.hasPrec {
		for len(s) < p.prec {
			s = "0" + s
		}
	}
	if neg {
		s = "-" + s
	} else if p.plus {
		s = "+" + s
	} else if p.space {
		s = " " + s
	}
	p.pad(s)
}

func upper(s string) string {
	b := []byte(s)
	for i, c := range b {
		if c >= 'a' && c <= 'z' {
			b[i] = c - 32
		}
	}
	return string(b)
}

func (p *printer) fmtFloat(f float64, bits int, verb rune) {
	format := byte('g')
	prec := -1
	switch verb {
	case 'e', 'E', 'f', 'g', 'G':
		format = byte(verb)
		if verb != 'g' && verb != 'G' {
			prec = 6
		}
	case 'F':
		format = 'f'
		prec = 6
	}
	if p.hasPrec {
		prec = p.prec
	}
	s := strconv.FormatFloat(f, format, prec, bits)
	if f >= 0 && s[0] != '+' {
		if p.plus {
			s = "+" + s
		} else if p.space {
			s = " " + s
		}
	}
	p.pad(s)
}

func (p *printer) fmtString(s string, verb rune) {
	switch verb {
	case 'q':
		s = strconv.Quote(s)
	case 'x', 'X':
		const hex = "0123456789abcdef"
		var out []byte
		for i := 0; i < len(s); i++ {
			out = append(out, hex[s[i]>>4], hex[s[i]&15])
		}
		s = string(out)
		if verb == 'X' {
			s = upper(s)
		}
	default:
		if p.hasPrec && p.prec < len(s) {
			s = s[:p.prec]
		}
	}
	p.pad(s)
}

// value formats one operand. depth is the nesting level inside a composite
// value; strings nested in a composite are not padded.
func (p *printer) value(v any, verb rune, depth int) {
	if v == nil {
		p.pad("<nil>")
		return
	}
	if verb == 'T' {
		var name string
		typeName(v, &name)
		p.pad(name)
		return
	}
	// Values that describe themselves.
	if verb == 'v' || verb == 's' || verb == 'q' {
		switch x := v.(type) {
		case error:
			p.fmtString(x.Error(), verb)
			return
		case Stringer:
			p.fmtString(x.String(), verb)
			return
		}
	}
	switch kind(v) {
	case kindBool:
		if verb != 'v' && verb != 't' {
			p.badVerb(verb, v)
			return
		}
		p.pad(strconv.FormatBool(boolOf(v)))
	case kindInt:
		n := intOf(v)
		switch verb {
		case 'v', 'd', 'b', 'o', 'x', 'X', 'c', 'q':
			if n < 0 {
				p.fmtInt(true, uint64(-n), verb)
			} else {
				p.fmtInt(false, uint64(n), verb)
			}
		case 'e', 'E', 'f', 'F', 'g', 'G':
			p.badVerb(verb, v)
		default:
			p.badVerb(verb, v)
		}
	case kindUint:
		switch verb {
		case 'v', 'd', 'b', 'o', 'x', 'X', 'c', 'q':
			p.fmtInt(false, uintOf(v), verb)
		default:
			p.badVerb(verb, v)
		}
	case kindFloat:
		switch verb {
		case 'v', 'e', 'E', 'f', 'F', 'g', 'G':
			p.fmtFloat(floatOf(v), size(v)*8, verb)
		default:
			p.badVerb(verb, v)
		}
	case kindString:
		var s string
		stringOf(v, &s)
		switch verb {
		case 'v', 's', 'q', 'x', 'X':
			p.fmtString(s, verb)
		default:
			p.badVerb(verb, v)
		}
	case kindPointer:
		addr := pointerOf(v)
		if addr == 0 {
			p.pad("<nil>")
			return
		}
		if depth == 0 && verb == 'v' {
			switch elemKind(v) {
			case kindStruct, kindSlice, kindArray, kindMap:
				var target any
				elem(v, 0, &target)
				p.writeString("&")
				p.value(target, verb, depth+1)
				return
			}
		}
		p.pad("0x" + strconv.FormatUint(addr, 16))
	case kindSlice, kindArray:
		if verb == 's' || verb == 'x' || verb == 'X' || verb == 'q' {
			if elemKind(v) == kindUint && size1(v) {
				p.fmtString(bytesOf(v), verb)
				return
			}
		}
		p.writeString("[")
		n := length(v)
		for i := 0; i < n; i++ {
			if i > 0 {
				p.writeString(" ")
			}
			var e any
			elem(v, i, &e)
			p.value(e, verb, depth+1)
		}
		p.writeString("]")
	case kindStruct:
		p.writeString("{")
		n := length(v)
		for i := 0; i < n; i++ {
			if i > 0 {
				p.writeString(" ")
			}
			if p.plusV || p.sharp {
				var name string
				fieldName(v, i, &name)
				p.writeString(name)
				p.writeString(":")
			}
			var e any
			elem(v, i, &e)
			p.value(e, verb, depth+1)
		}
		p.writeString("}")
	case kindMap:
		p.writeString("map[")
		cursor := 0
		first := true
		for {
			var k, e any
			if !mapNext(v, &cursor, &k, &e) {
				break
			}
			if !first {
				p.writeString(" ")
			}
			first = false
			p.value(k, verb, depth+1)
			p.writeString(":")
			p.value(e, verb, depth+1)
		}
		p.writeString("]")
	case kindFunc:
		p.pad("0x" + strconv.FormatUint(pointerOf(v), 16))
	default:
		var name string
		typeName(v, &name)
		p.pad("<" + name + ">")
	}
}

// size1 reports whether the elements of the slice v are single bytes.
func size1(v any) bool {
	if length(v) == 0 {
		return true
	}
	var e any
	elem(v, 0, &e)
	return size(e) == 1
}

func bytesOf(v any) string {
	n := length(v)
	out := make([]byte, n)
	for i := 0; i < n; i++ {
		var e any
		elem(v, i, &e)
		out[i] = byte(uintOf(e))
	}
	return string(out)
}

func isString(v any) bool { return v != nil && kind(v) == kindString }

func (p *printer) clearFlags() {
	p.minus, p.plus, p.sharp, p.space, p.zero = false, false, false, false, false
	p.plusV = false
	p.hasWidth, p.hasPrec = false, false
	p.width, p.prec = 0, 0
}

// print writes the operands; a space goes between two operands when
// neither is a string.
func (p *printer) print(args []any) {
	for i, a := range args {
		if i > 0 && !isString(a) && !isString(args[i-1]) {
			p.buf = append(p.buf, ' ')
		}
		p.value(a, 'v', 0)
	}
}

func (p *printer) println(args []any) {
	for i, a := range args {
		if i > 0 {
			p.buf = append(p.buf, ' ')
		}
		p.value(a, 'v', 0)
	}
	p.buf = append(p.buf, '\n')
}

// printf formats according to the verbs in format. wrapped receives the
// operand of a %w verb, if there is one.
func (p *printer) printf(format string, args []any, wrapped *error) {
	next := 0
	i := 0
	for i < len(format) {
		c := format[i]
		if c != '%' {
			start := i
			for i < len(format) && format[i] != '%' {
				i++
			}
			p.writeString(format[start:i])
			continue
		}
		i++
		p.clearFlags()
		// Flags.
		for i < len(format) {
			done := false
			switch format[i] {
			case '-':
				p.minus = true
			case '+':
				p.plus = true
			case '#':
				p.sharp = true
			case ' ':
				p.space = true
			case '0':
				p.zero = true
			default:
				done = true
			}
			if done {
				break
			}
			i++
		}
		// Width.
		if i < len(format) && format[i] == '*' {
			i++
			if next < len(args) && kind(args[next]) == kindInt {
				p.width = int(intOf(args[next]))
				p.hasWidth = true
				if p.width < 0 {
					p.width = -p.width
					p.minus = true
				}
			}
			next++
		} else {
			for i < len(format) && format[i] >= '0' && format[i] <= '9' {
				p.width = p.width*10 + int(format[i]-'0')
				p.hasWidth = true
				i++
			}
		}
		// Precision.
		if i < len(format) && format[i] == '.' {
			i++
			p.hasPrec = true
			if i < len(format) && format[i] == '*' {
				i++
				if next < len(args) && kind(args[next]) == kindInt {
					p.prec = int(intOf(args[next]))
				}
				next++
			} else {
				for i < len(format) && format[i] >= '0' && format[i] <= '9' {
					p.prec = p.prec*10 + int(format[i]-'0')
					i++
				}
			}
		}
		if i >= len(format) {
			p.writeString("%!(NOVERB)")
			break
		}
		// The verb is one rune.
		verb := rune(format[i])
		size := 1
		if verb >= 0x80 {
			for _, r := range format[i:] {
				verb = r
				size = len(string(r))
				break
			}
		}
		i += size
		if verb == '%' {
			p.buf = append(p.buf, '%')
			continue
		}
		if next >= len(args) {
			p.writeString("%!")
			p.writeString(string(verb))
			p.writeString("(MISSING)")
			continue
		}
		arg := args[next]
		next++
		if verb == 'w' {
			if err, ok := arg.(error); ok && wrapped != nil {
				*wrapped = err
			}
			verb = 'v'
		}
		if verb == 'p' {
			if arg != nil && (kind(arg) == kindPointer || kind(arg) == kindMap || kind(arg) == kindFunc) {
				p.pad("0x" + strconv.FormatUint(pointerOf(arg), 16))
			} else {
				p.badVerb(verb, arg)
			}
			continue
		}
		if verb == 'v' && p.plus {
			// For %+v the flag selects field names, not a sign on numbers.
			p.plus = false
			p.plusV = true
		}
		p.value(arg, verb, 0)
	}
	if next < len(args) {
		p.clearFlags()
		p.writeString("%!(EXTRA ")
		for j := next; j < len(args); j++ {
			if j > next {
				p.writeString(", ")
			}
			var name string
			typeName(args[j], &name)
			p.writeString(name)
			p.writeString("=")
			p.value(args[j], 'v', 0)
		}
		p.writeString(")")
	}
}

func Sprint(a ...any) string {
	p := &printer{}
	p.print(a)
	return string(p.buf)
}

func Sprintln(a ...any) string {
	p := &printer{}
	p.println(a)
	return string(p.buf)
}

func Sprintf(format string, a ...any) string {
	p := &printer{}
	p.printf(format, a, nil)
	return string(p.buf)
}

func Fprint(w io.Writer, a ...any) (n int, err error) {
	p := &printer{}
	p.print(a)
	return w.Write(p.buf)
}

func Fprintln(w io.Writer, a ...any) (n int, err error) {
	p := &printer{}
	p.println(a)
	return w.Write(p.buf)
}

func Fprintf(w io.Writer, format string, a ...any) (n int, err error) {
	p := &printer{}
	p.printf(format, a, nil)
	return w.Write(p.buf)
}

func Print(a ...any) (n int, err error) { return Fprint(os.Stdout, a...) }

func Println(a ...any) (n int, err error) { return Fprintln(os.Stdout, a...) }

func Printf(format string, a ...any) (n int, err error) {
	return Fprintf(os.Stdout, format, a...)
}

type wrapError struct {
	msg string
	err error
}

func (e *wrapError) Error() string { return e.msg }
func (e *wrapError) Unwrap() error { return e.err }

type plainError struct {
	msg string
}

func (e *plainError) Error() string { return e.msg }

// Errorf formats an error message. With a %w verb the returned error wraps
// that operand, so errors.Is and errors.Unwrap can reach it.
func Errorf(format string, a ...any) error {
	p := &printer{}
	var wrapped error
	p.printf(format, a, &wrapped)
	if wrapped != nil {
		return &wrapError{string(p.buf), wrapped}
	}
	return &plainError{string(p.buf)}
}
