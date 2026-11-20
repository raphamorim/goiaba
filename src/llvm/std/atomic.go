package atomic

// Goroutines run one at a time and switch only where they block, so plain
// memory operations are already atomic.

func AddInt32(addr *int32, delta int32) int32 {
	*addr += delta
	return *addr
}

func AddInt64(addr *int64, delta int64) int64 {
	*addr += delta
	return *addr
}

func AddUint32(addr *uint32, delta uint32) uint32 {
	*addr += delta
	return *addr
}

func AddUint64(addr *uint64, delta uint64) uint64 {
	*addr += delta
	return *addr
}

func LoadInt32(addr *int32) int32    { return *addr }
func LoadInt64(addr *int64) int64    { return *addr }
func LoadUint32(addr *uint32) uint32 { return *addr }
func LoadUint64(addr *uint64) uint64 { return *addr }

func StoreInt32(addr *int32, val int32)    { *addr = val }
func StoreInt64(addr *int64, val int64)    { *addr = val }
func StoreUint32(addr *uint32, val uint32) { *addr = val }
func StoreUint64(addr *uint64, val uint64) { *addr = val }

func SwapInt32(addr *int32, new int32) int32 {
	old := *addr
	*addr = new
	return old
}

func SwapInt64(addr *int64, new int64) int64 {
	old := *addr
	*addr = new
	return old
}

func CompareAndSwapInt32(addr *int32, old, new int32) bool {
	if *addr != old {
		return false
	}
	*addr = new
	return true
}

func CompareAndSwapInt64(addr *int64, old, new int64) bool {
	if *addr != old {
		return false
	}
	*addr = new
	return true
}
