package time

func sleep(ns int64)
func monotonic() int64
func unixNano() int64

// Duration is an elapsed time in nanoseconds.
type Duration int64

const (
	Nanosecond  Duration = 1
	Microsecond          = 1000 * Nanosecond
	Millisecond          = 1000 * Microsecond
	Second               = 1000 * Millisecond
	Minute               = 60 * Second
	Hour                 = 60 * Minute
)

func (d Duration) Nanoseconds() int64  { return int64(d) }
func (d Duration) Microseconds() int64 { return int64(d) / 1000 }
func (d Duration) Milliseconds() int64 { return int64(d) / 1000000 }
func (d Duration) Seconds() float64    { return float64(d) / 1e9 }
func (d Duration) Minutes() float64    { return float64(d) / 6e10 }
func (d Duration) Hours() float64      { return float64(d) / 3.6e12 }

// Sleep pauses the current goroutine for at least d.
func Sleep(d Duration) { sleep(int64(d)) }

// Time is an instant: wall-clock nanoseconds since the Unix epoch, plus a
// monotonic reading used to measure elapsed time.
type Time struct {
	wall int64
	mono int64
}

func Now() Time { return Time{unixNano(), monotonic()} }

func (t Time) Unix() int64      { return t.wall / 1000000000 }
func (t Time) UnixMilli() int64 { return t.wall / 1000000 }
func (t Time) UnixNano() int64  { return t.wall }
func (t Time) IsZero() bool     { return t.wall == 0 }

// Sub returns the duration t-u.
func (t Time) Sub(u Time) Duration { return Duration(t.mono - u.mono) }

func (t Time) Before(u Time) bool { return t.mono < u.mono }
func (t Time) After(u Time) bool  { return t.mono > u.mono }

// Add returns the time t+d.
func (t Time) Add(d Duration) Time { return Time{t.wall + int64(d), t.mono + int64(d)} }

// Since returns the time elapsed since t.
func Since(t Time) Duration { return Now().Sub(t) }

// After waits for the duration to elapse and then sends the current time on
// the returned channel.
func After(d Duration) <-chan Time {
	c := make(chan Time, 1)
	go func() {
		Sleep(d)
		c <- Now()
	}()
	return c
}
