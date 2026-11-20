package strings

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

func Contains(s, substr string) bool { return Index(s, substr) >= 0 }

func HasPrefix(s, prefix string) bool {
	return len(s) >= len(prefix) && s[:len(prefix)] == prefix
}

func HasSuffix(s, suffix string) bool {
	return len(s) >= len(suffix) && s[len(s)-len(suffix):] == suffix
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
