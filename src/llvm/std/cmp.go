package cmp

// Ordered is a constraint for types that support the < operator.
type Ordered interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64 |
		~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 | ~uintptr |
		~float32 | ~float64 |
		~string
}

// Less reports whether x is less than y.
func Less[T Ordered](x, y T) bool { return x < y }

// Compare returns -1, 0 or +1 for x less than, equal to or greater than y.
func Compare[T Ordered](x, y T) int {
	if x < y {
		return -1
	}
	if x > y {
		return 1
	}
	return 0
}
