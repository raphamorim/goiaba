package slices

import "cmp"

// Index returns the index of the first occurrence of v in s, or -1.
func Index[E comparable](s []E, v E) int {
	for i := range s {
		if s[i] == v {
			return i
		}
	}
	return -1
}

// IndexFunc returns the first index i for which f(s[i]) is true, or -1.
func IndexFunc[E any](s []E, f func(E) bool) int {
	for i := range s {
		if f(s[i]) {
			return i
		}
	}
	return -1
}

func Contains[E comparable](s []E, v E) bool { return Index(s, v) >= 0 }

func ContainsFunc[E any](s []E, f func(E) bool) bool { return IndexFunc(s, f) >= 0 }

// Equal reports whether two slices have the same length and elements.
func Equal[E comparable](a, b []E) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// Reverse reverses the elements of the slice in place.
func Reverse[E any](s []E) {
	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		s[i], s[j] = s[j], s[i]
	}
}

// Max returns the largest element. It panics if the slice is empty.
func Max[E cmp.Ordered](s []E) E {
	if len(s) == 0 {
		panic("slices.Max: empty list")
	}
	m := s[0]
	for _, v := range s[1:] {
		if v > m {
			m = v
		}
	}
	return m
}

// Min returns the smallest element. It panics if the slice is empty.
func Min[E cmp.Ordered](s []E) E {
	if len(s) == 0 {
		panic("slices.Min: empty list")
	}
	m := s[0]
	for _, v := range s[1:] {
		if v < m {
			m = v
		}
	}
	return m
}

// SortFunc sorts the slice in ascending order as determined by compare,
// which returns a negative number when a comes before b. The sort is stable.
func SortFunc[E any](s []E, compare func(a, b E) int) {
	// Merge sort.
	if len(s) < 2 {
		return
	}
	mid := len(s) / 2
	left := Clone(s[:mid])
	right := Clone(s[mid:])
	SortFunc(left, compare)
	SortFunc(right, compare)
	i, j := 0, 0
	for k := range s {
		if j >= len(right) || i < len(left) && compare(left[i], right[j]) <= 0 {
			s[k] = left[i]
			i++
		} else {
			s[k] = right[j]
			j++
		}
	}
}

// Sort sorts a slice of any ordered type in ascending order.
func Sort[E cmp.Ordered](s []E) {
	SortFunc(s, cmp.Compare[E])
}

// IsSorted reports whether the slice is in ascending order.
func IsSorted[E cmp.Ordered](s []E) bool {
	for i := 1; i < len(s); i++ {
		if s[i] < s[i-1] {
			return false
		}
	}
	return true
}

// Clone returns a copy of the slice.
func Clone[E any](s []E) []E {
	if s == nil {
		return nil
	}
	return append([]E{}, s...)
}
