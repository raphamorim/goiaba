package sync

func semacquire(addr *uint32)
func semrelease(addr *uint32)

// Goroutines are scheduled cooperatively and one at a time, so the state
// below needs no atomic operations: a goroutine is only interrupted where it
// blocks.

// Mutex is a mutual exclusion lock. The zero value is unlocked.
type Mutex struct {
	locked  bool
	waiters int
	sema    uint32
}

func (m *Mutex) Lock() {
	if !m.locked {
		m.locked = true
		return
	}
	m.waiters++
	// Unlock hands the lock over directly, so it stays locked on wake-up.
	semacquire(&m.sema)
}

func (m *Mutex) TryLock() bool {
	if m.locked {
		return false
	}
	m.locked = true
	return true
}

func (m *Mutex) Unlock() {
	if !m.locked {
		panic("sync: unlock of unlocked mutex")
	}
	if m.waiters > 0 {
		m.waiters--
		semrelease(&m.sema)
		return
	}
	m.locked = false
}

// Locker is implemented by Mutex and RWMutex.
type Locker interface {
	Lock()
	Unlock()
}

// RWMutex is a reader/writer lock: many readers or one writer.
type RWMutex struct {
	w       Mutex
	readers int
	waiting bool
	sema    uint32
}

func (rw *RWMutex) RLock() {
	// Wait for a writer that holds or is taking the lock.
	rw.w.Lock()
	rw.readers++
	rw.w.Unlock()
}

func (rw *RWMutex) RUnlock() {
	if rw.readers == 0 {
		panic("sync: RUnlock of unlocked RWMutex")
	}
	rw.readers--
	if rw.readers == 0 && rw.waiting {
		rw.waiting = false
		semrelease(&rw.sema)
	}
}

func (rw *RWMutex) Lock() {
	rw.w.Lock()
	if rw.readers > 0 {
		rw.waiting = true
		semacquire(&rw.sema)
	}
}

func (rw *RWMutex) Unlock() { rw.w.Unlock() }

// WaitGroup waits for a collection of goroutines to finish.
type WaitGroup struct {
	count   int
	waiters int
	sema    uint32
}

func (wg *WaitGroup) Add(delta int) {
	wg.count += delta
	if wg.count < 0 {
		panic("sync: negative WaitGroup counter")
	}
	if wg.count == 0 {
		for wg.waiters > 0 {
			wg.waiters--
			semrelease(&wg.sema)
		}
	}
}

func (wg *WaitGroup) Done() { wg.Add(-1) }

func (wg *WaitGroup) Wait() {
	if wg.count == 0 {
		return
	}
	wg.waiters++
	semacquire(&wg.sema)
}

// Once runs a function exactly once.
type Once struct {
	done bool
	m    Mutex
}

func (o *Once) Do(f func()) {
	if o.done {
		return
	}
	o.m.Lock()
	defer o.m.Unlock()
	if !o.done {
		defer func() { o.done = true }()
		f()
	}
}
