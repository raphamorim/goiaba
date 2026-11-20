package unicode

// This package covers ASCII and Latin-1 only.

const (
	MaxRune         = 0x10FFFF
	ReplacementChar = 0xFFFD
)

func IsUpper(r rune) bool {
	return r >= 'A' && r <= 'Z' || r >= 0xC0 && r <= 0xDE && r != 0xD7
}

func IsLower(r rune) bool {
	return r >= 'a' && r <= 'z' || r >= 0xDF && r <= 0xFF && r != 0xF7
}

func IsLetter(r rune) bool {
	return IsUpper(r) || IsLower(r) || r == 0xAA || r == 0xB5 || r == 0xBA
}

func IsDigit(r rune) bool { return r >= '0' && r <= '9' }

func IsNumber(r rune) bool { return IsDigit(r) }

func IsSpace(r rune) bool {
	switch r {
	case ' ', '\t', '\n', '\v', '\f', '\r', 0x85, 0xA0:
		return true
	}
	return false
}

func IsPunct(r rune) bool {
	switch {
	case r >= '!' && r <= '/', r >= ':' && r <= '@', r >= '[' && r <= '`', r >= '{' && r <= '~':
		return r != '$' && r != '+' && r != '<' && r != '=' && r != '>' && r != '^' && r != '`' && r != '|' && r != '~'
	}
	return false
}

func IsControl(r rune) bool { return r < 0x20 || r >= 0x7F && r < 0xA0 }

func IsPrint(r rune) bool { return r >= 0x20 && r < 0x7F || r > 0xA0 && r <= 0xFF && r != 0xAD }

func ToUpper(r rune) rune {
	if r >= 'a' && r <= 'z' || r >= 0xE0 && r <= 0xFE && r != 0xF7 {
		return r - 32
	}
	return r
}

func ToLower(r rune) rune {
	if r >= 'A' && r <= 'Z' || r >= 0xC0 && r <= 0xDE && r != 0xD7 {
		return r + 32
	}
	return r
}
