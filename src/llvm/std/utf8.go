package utf8

const (
	RuneError = 0xFFFD
	RuneSelf  = 0x80
	UTFMax    = 4
)

// RuneLen returns the number of bytes needed to encode the rune, or -1.
func RuneLen(r rune) int {
	if r < 0 {
		return -1
	}
	if r < 0x80 {
		return 1
	}
	if r < 0x800 {
		return 2
	}
	if r >= 0xD800 && r <= 0xDFFF {
		return -1
	}
	if r < 0x10000 {
		return 3
	}
	if r <= 0x10FFFF {
		return 4
	}
	return -1
}

// RuneCountInString returns the number of runes in s.
func RuneCountInString(s string) int {
	n := 0
	for range s {
		n++
	}
	return n
}

// RuneCount returns the number of runes in p.
func RuneCount(p []byte) int {
	return RuneCountInString(string(p))
}

// ValidString reports whether s consists entirely of valid UTF-8.
func ValidString(s string) bool {
	for i, r := range s {
		if r == RuneError {
			// A real U+FFFD is three bytes; a decoding error is one.
			if i+3 > len(s) || s[i] != 0xEF || s[i+1] != 0xBF || s[i+2] != 0xBD {
				return false
			}
		}
	}
	return true
}

// AppendRune appends the UTF-8 encoding of r to p.
func AppendRune(p []byte, r rune) []byte {
	return append(p, string(r)...)
}
