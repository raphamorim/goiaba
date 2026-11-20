package strconv

import "errors"

func formatFloat(f float64, fmt int, prec int, bitSize int, buf []byte) int
func parseFloat(s string, out *float64) bool

// ErrSyntax means a value does not have the right syntax for the target type.
var ErrSyntax = errors.New("invalid syntax")

// ErrRange means a value is out of range for the target type.
var ErrRange = errors.New("value out of range")

// NumError records a failed conversion.
type NumError struct {
	Func string
	Num  string
	Err  error
}

func (e *NumError) Error() string {
	return "strconv." + e.Func + ": parsing " + Quote(e.Num) + ": " + e.Err.Error()
}

func (e *NumError) Unwrap() error { return e.Err }

const digits = "0123456789abcdefghijklmnopqrstuvwxyz"

// FormatUint returns the string representation of i in the given base.
func FormatUint(i uint64, base int) string {
	if base < 2 || base > 36 {
		panic("strconv: illegal number base")
	}
	if i == 0 {
		return "0"
	}
	var buf [64]byte
	n := len(buf)
	b := uint64(base)
	for i > 0 {
		n--
		buf[n] = digits[i%b]
		i /= b
	}
	return string(buf[n:])
}

// FormatInt returns the string representation of i in the given base.
func FormatInt(i int64, base int) string {
	if i < 0 {
		return "-" + FormatUint(uint64(-i), base)
	}
	return FormatUint(uint64(i), base)
}

func Itoa(i int) string { return FormatInt(int64(i), 10) }

func FormatBool(b bool) string {
	if b {
		return "true"
	}
	return "false"
}

// FormatFloat converts f to a string. fmt is 'e', 'f' or 'g'; a negative
// precision selects the shortest representation that parses back to f.
func FormatFloat(f float64, fmt byte, prec, bitSize int) string {
	buf := make([]byte, 400)
	n := formatFloat(f, int(fmt), prec, bitSize, buf)
	return string(buf[:n])
}

// ParseUint parses an unsigned integer in the given base (0 means: read the
// base from a 0x, 0o or 0b prefix).
func ParseUint(s string, base int, bitSize int) (uint64, error) {
	orig := s
	if s == "" {
		return 0, &NumError{"ParseUint", orig, ErrSyntax}
	}
	if base == 0 {
		base = 10
		if len(s) > 1 && s[0] == '0' {
			switch s[1] {
			case 'x', 'X':
				base = 16
				s = s[2:]
			case 'b', 'B':
				base = 2
				s = s[2:]
			case 'o', 'O':
				base = 8
				s = s[2:]
			default:
				base = 8
				s = s[1:]
			}
		}
	}
	if bitSize == 0 {
		bitSize = 64
	}
	var max uint64 = 1<<64 - 1
	if bitSize < 64 {
		max = 1<<uint(bitSize) - 1
	}
	var n uint64
	for i := 0; i < len(s); i++ {
		c := s[i]
		var d byte
		switch {
		case c == '_' && base != 10:
			continue
		case c >= '0' && c <= '9':
			d = c - '0'
		case c >= 'a' && c <= 'z':
			d = c - 'a' + 10
		case c >= 'A' && c <= 'Z':
			d = c - 'A' + 10
		default:
			return 0, &NumError{"ParseUint", orig, ErrSyntax}
		}
		if int(d) >= base {
			return 0, &NumError{"ParseUint", orig, ErrSyntax}
		}
		if n > (max-uint64(d))/uint64(base) {
			return max, &NumError{"ParseUint", orig, ErrRange}
		}
		n = n*uint64(base) + uint64(d)
	}
	if len(s) == 0 {
		return 0, &NumError{"ParseUint", orig, ErrSyntax}
	}
	return n, nil
}

// ParseInt parses a signed integer in the given base.
func ParseInt(s string, base int, bitSize int) (int64, error) {
	orig := s
	if s == "" {
		return 0, &NumError{"ParseInt", orig, ErrSyntax}
	}
	neg := false
	if s[0] == '+' {
		s = s[1:]
	} else if s[0] == '-' {
		neg = true
		s = s[1:]
	}
	un, err := ParseUint(s, base, bitSize)
	if err != nil {
		ne := err.(*NumError)
		if ne.Err != ErrRange {
			return 0, &NumError{"ParseInt", orig, ne.Err}
		}
	}
	if bitSize == 0 {
		bitSize = 64
	}
	cutoff := uint64(1) << uint(bitSize-1)
	if !neg && (un >= cutoff || err != nil) {
		return int64(cutoff - 1), &NumError{"ParseInt", orig, ErrRange}
	}
	if neg && (un > cutoff || err != nil) {
		return -int64(cutoff), &NumError{"ParseInt", orig, ErrRange}
	}
	if neg {
		return -int64(un), nil
	}
	return int64(un), nil
}

// Atoi is ParseInt(s, 10, 0) converted to int.
func Atoi(s string) (int, error) {
	n, err := ParseInt(s, 10, 0)
	if err != nil {
		ne := err.(*NumError)
		return int(n), &NumError{"Atoi", s, ne.Err}
	}
	return int(n), nil
}

func ParseBool(str string) (bool, error) {
	switch str {
	case "1", "t", "T", "true", "TRUE", "True":
		return true, nil
	case "0", "f", "F", "false", "FALSE", "False":
		return false, nil
	}
	return false, &NumError{"ParseBool", str, ErrSyntax}
}

func ParseFloat(s string, bitSize int) (float64, error) {
	var f float64
	if !parseFloat(s, &f) {
		return 0, &NumError{"ParseFloat", s, ErrSyntax}
	}
	return f, nil
}

// Quote returns a double-quoted Go string literal representing s.
func Quote(s string) string {
	out := []byte{'"'}
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch {
		case c == '"' || c == '\\':
			out = append(out, '\\', c)
		case c == '\n':
			out = append(out, '\\', 'n')
		case c == '\t':
			out = append(out, '\\', 't')
		case c == '\r':
			out = append(out, '\\', 'r')
		case c < 0x20 || c == 0x7F:
			out = append(out, '\\', 'x', digits[c>>4], digits[c&0xF])
		default:
			out = append(out, c)
		}
	}
	return string(append(out, '"'))
}
