package sort

func length(x any) int
func swap(x any, i, j int)

// Interface is implemented by collections that can be sorted.
type Interface interface {
	Len() int
	Less(i, j int) bool
	Swap(i, j int)
}

// heapSort sorts n elements using less and swap. It is O(n log n) and
// in place, but not stable.
func heapSort(n int, less func(i, j int) bool, swap func(i, j int)) {
	siftDown := func(root, end int) {
		for {
			child := 2*root + 1
			if child >= end {
				return
			}
			if child+1 < end && less(child, child+1) {
				child++
			}
			if !less(root, child) {
				return
			}
			swap(root, child)
			root = child
		}
	}
	for i := n/2 - 1; i >= 0; i-- {
		siftDown(i, n)
	}
	for end := n - 1; end > 0; end-- {
		swap(0, end)
		siftDown(0, end)
	}
}

// insertionSort is stable.
func insertionSort(n int, less func(i, j int) bool, swap func(i, j int)) {
	for i := 1; i < n; i++ {
		for j := i; j > 0 && less(j, j-1); j-- {
			swap(j, j-1)
		}
	}
}

// Sort sorts data in ascending order as determined by Less.
func Sort(data Interface) {
	heapSort(data.Len(), data.Less, data.Swap)
}

// Stable sorts data and keeps equal elements in their original order.
func Stable(data Interface) {
	insertionSort(data.Len(), data.Less, data.Swap)
}

// Slice sorts the slice x given the less function.
func Slice(x any, less func(i, j int) bool) {
	heapSort(length(x), less, func(i, j int) { swap(x, i, j) })
}

// SliceStable sorts the slice x and keeps equal elements in order.
func SliceStable(x any, less func(i, j int) bool) {
	insertionSort(length(x), less, func(i, j int) { swap(x, i, j) })
}

func Ints(x []int) {
	heapSort(len(x), func(i, j int) bool { return x[i] < x[j] }, func(i, j int) { x[i], x[j] = x[j], x[i] })
}

func Strings(x []string) {
	heapSort(len(x), func(i, j int) bool { return x[i] < x[j] }, func(i, j int) { x[i], x[j] = x[j], x[i] })
}

func Float64s(x []float64) {
	heapSort(len(x), func(i, j int) bool { return x[i] < x[j] }, func(i, j int) { x[i], x[j] = x[j], x[i] })
}

// SearchInts returns the index of x in the sorted slice a, or where it
// would be inserted.
func SearchInts(a []int, x int) int {
	lo, hi := 0, len(a)
	for lo < hi {
		mid := (lo + hi) / 2
		if a[mid] < x {
			lo = mid + 1
		} else {
			hi = mid
		}
	}
	return lo
}

func IntsAreSorted(x []int) bool {
	for i := 1; i < len(x); i++ {
		if x[i] < x[i-1] {
			return false
		}
	}
	return true
}
