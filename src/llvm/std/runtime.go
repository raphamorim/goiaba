package runtime

func gc()
func readMemStats(m *MemStats)
func gosched()
func numGoroutine() int

// MemStats reports memory allocator statistics. Only these fields exist.
type MemStats struct {
	Alloc      uint64 // bytes of allocated heap objects
	TotalAlloc uint64 // cumulative bytes allocated
	HeapAlloc  uint64 // same as Alloc
	NumGC      uint32 // completed collections
}

// GC runs a garbage collection and blocks until it is done.
func GC() { gc() }

// ReadMemStats fills m with allocator statistics.
func ReadMemStats(m *MemStats) { readMemStats(m) }

// Gosched lets other goroutines run. Scheduling is cooperative: a goroutine
// keeps the processor until it blocks or calls Gosched.
func Gosched() { gosched() }

// NumGoroutine returns the number of goroutines that exist.
func NumGoroutine() int { return numGoroutine() }
