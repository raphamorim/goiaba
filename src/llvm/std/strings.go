package strings

import "unicode"

// Index returns the index of the first instance of substr in s, or -1.
func Index(s, substr string) int {
	n := len(substr)
	for i := 0; i+n <= len(s); i++ {
		if s[i:i+n] == substr {
			return i
		}
	}
	return -1
}

// LastIndex returns the index of the last instance of substr in s, or -1.
func LastIndex(s, substr string) int {
	n := len(substr)
	for i := len(s) - n; i >= 0; i-- {
		if s[i:i+n] == substr {
			return i
		}
	}
	return -1
}

func IndexByte(s string, c byte) int {
	for i := 0; i < len(s); i++ {
		if s[i] == c {
			return i
		}
	}
	return -1
}

func IndexRune(s string, r rune) int {
	for i, c := range s {
		if c == r {
			return i
		}
	}
	return -1
}

func Contains(s, substr string) bool { return Index(s, substr) >= 0 }

func ContainsRune(s string, r rune) bool { return IndexRune(s, r) >= 0 }

func HasPrefix(s, prefix string) bool {
	return len(s) >= len(prefix) && s[:len(prefix)] == prefix
}

func HasSuffix(s, suffix string) bool {
	return len(s) >= len(suffix) && s[len(s)-len(suffix):] == suffix
}

func TrimPrefix(s, prefix string) string {
	if HasPrefix(s, prefix) {
		return s[len(prefix):]
	}
	return s
}

func TrimSuffix(s, suffix string) string {
	if HasSuffix(s, suffix) {
		return s[:len(s)-len(suffix)]
	}
	return s
}

// Count counts the non-overlapping instances of substr in s.
func Count(s, substr string) int {
	if len(substr) == 0 {
		n := 1
		for range s {
			n++
		}
		return n
	}
	n := 0
	for {
		i := Index(s, substr)
		if i < 0 {
			return n
		}
		n++
		s = s[i+len(substr):]
	}
}

// Split slices s into the substrings separated by sep.
func Split(s, sep string) []string {
	var out []string
	if sep == "" {
		for _, r := range s {
			out = append(out, string(r))
		}
		return out
	}
	for {
		i := Index(s, sep)
		if i < 0 {
			break
		}
		out = append(out, s[:i])
		s = s[i+len(sep):]
	}
	return append(out, s)
}

// Fields splits s around runs of white space.
func Fields(s string) []string {
	var out []string
	start := -1
	for i, r := range s {
		if unicode.IsSpace(r) {
			if start >= 0 {
				out = append(out, s[start:i])
				start = -1
			}
		} else if start < 0 {
			start = i
		}
	}
	if start >= 0 {
		out = append(out, s[start:])
	}
	return out
}

// Join concatenates the elements with sep between them.
func Join(elems []string, sep string) string {
	var out []byte
	for i, e := range elems {
		if i > 0 {
			out = append(out, sep...)
		}
		out = append(out, e...)
	}
	return string(out)
}

func Repeat(s string, count int) string {
	if count < 0 {
		panic("strings: negative Repeat count")
	}
	var out []byte
	for i := 0; i < count; i++ {
		out = append(out, s...)
	}
	return string(out)
}

// Replace returns a copy of s with the first n instances of old replaced by
// new. If n < 0 there is no limit.
func Replace(s, old, new string, n int) string {
	if old == "" || n == 0 {
		return s
	}
	var out []byte
	for n != 0 {
		i := Index(s, old)
		if i < 0 {
			break
		}
		out = append(out, s[:i]...)
		out = append(out, new...)
		s = s[i+len(old):]
		n--
	}
	out = append(out, s...)
	return string(out)
}

func ReplaceAll(s, old, new string) string { return Replace(s, old, new, -1) }

// Map returns a copy of s with every rune replaced by mapping(rune).
// Runes for which mapping returns a negative value are dropped.
func Map(mapping func(rune) rune, s string) string {
	var out []byte
	for _, r := range s {
		m := mapping(r)
		if m >= 0 {
			out = append(out, string(m)...)
		}
	}
	return string(out)
}

func ToUpper(s string) string { return Map(unicode.ToUpper, s) }

func ToLower(s string) string { return Map(unicode.ToLower, s) }

// TrimFunc trims the leading and trailing runes that satisfy f.
func TrimFunc(s string, f func(rune) bool) string {
	start := len(s)
	for i, r := range s {
		if !f(r) {
			start = i
			break
		}
	}
	end := start
	for i, r := range s[start:] {
		if !f(r) {
			end = start + i + len(string(r))
		}
	}
	return s[start:end]
}

func TrimSpace(s string) string { return TrimFunc(s, unicode.IsSpace) }

// Trim trims the leading and trailing runes contained in cutset.
func Trim(s, cutset string) string {
	return TrimFunc(s, func(r rune) bool { return ContainsRune(cutset, r) })
}

func TrimLeft(s, cutset string) string {
	for i, r := range s {
		if !ContainsRune(cutset, r) {
			return s[i:]
		}
	}
	return ""
}

func TrimRight(s, cutset string) string {
	end := 0
	for i, r := range s {
		if !ContainsRune(cutset, r) {
			end = i + len(string(r))
		}
	}
	return s[:end]
}

// EqualFold reports whether s and t are equal ignoring letter case.
func EqualFold(s, t string) bool {
	return ToLower(s) == ToLower(t)
}

// Compare returns -1, 0 or +1 for s less than, equal to or greater than t.
func Compare(a, b string) int {
	if a < b {
		return -1
	}
	if a > b {
		return 1
	}
	return 0
}

// Builder builds a string from pieces without copying it at each step.
type Builder struct {
	buf []byte
}

func (b *Builder) WriteString(s string) (int, error) {
	b.buf = append(b.buf, s...)
	return len(s), nil
}

func (b *Builder) WriteByte(c byte) error {
	b.buf = append(b.buf, c)
	return nil
}

func (b *Builder) WriteRune(r rune) (int, error) {
	s := string(r)
	b.buf = append(b.buf, s...)
	return len(s), nil
}

func (b *Builder) Write(p []byte) (int, error) {
	b.buf = append(b.buf, p...)
	return len(p), nil
}

func (b *Builder) String() string { return string(b.buf) }

func (b *Builder) Len() int { return len(b.buf) }

func (b *Builder) Reset() { b.buf = nil }
