package math

const (
	Pi = 3.14159265358979323846264338327950288419716939937510582097494459
	E  = 2.71828182845904523536028747135266249775724709369995957496696763

	MaxInt8   = 1<<7 - 1
	MinInt8   = -1 << 7
	MaxInt16  = 1<<15 - 1
	MinInt16  = -1 << 15
	MaxInt32  = 1<<31 - 1
	MinInt32  = -1 << 31
	MaxInt64  = 1<<63 - 1
	MinInt64  = -1 << 63
	MaxInt    = MaxInt64
	MinInt    = MinInt64
	MaxUint8  = 1<<8 - 1
	MaxUint16 = 1<<16 - 1
	MaxUint32 = 1<<32 - 1

	MaxFloat64 = 1.79769313486231570814527423731704356798070e+308
)

func sqrt(x float64) float64
func floor(x float64) float64
func ceil(x float64) float64
func trunc(x float64) float64
func pow(x, y float64) float64
func exp(x float64) float64
func log(x float64) float64
func log2(x float64) float64
func log10(x float64) float64
func sin(x float64) float64
func cos(x float64) float64
func tan(x float64) float64
func atan(x float64) float64
func atan2(y, x float64) float64
func mod(x, y float64) float64
func inf(sign int) float64
func nan() float64

func Sqrt(x float64) float64     { return sqrt(x) }
func Floor(x float64) float64    { return floor(x) }
func Ceil(x float64) float64     { return ceil(x) }
func Trunc(x float64) float64    { return trunc(x) }
func Pow(x, y float64) float64   { return pow(x, y) }
func Exp(x float64) float64      { return exp(x) }
func Log(x float64) float64      { return log(x) }
func Log2(x float64) float64     { return log2(x) }
func Log10(x float64) float64    { return log10(x) }
func Sin(x float64) float64      { return sin(x) }
func Cos(x float64) float64      { return cos(x) }
func Tan(x float64) float64      { return tan(x) }
func Atan(x float64) float64     { return atan(x) }
func Atan2(y, x float64) float64 { return atan2(y, x) }
func Mod(x, y float64) float64   { return mod(x, y) }
func Inf(sign int) float64       { return inf(sign) }
func NaN() float64               { return nan() }

func IsNaN(f float64) bool { return f != f }

func IsInf(f float64, sign int) bool {
	return sign >= 0 && f > MaxFloat64 || sign <= 0 && f < -MaxFloat64
}

func Abs(x float64) float64 {
	if x < 0 {
		return -x
	}
	return x
}

func Max(x, y float64) float64 {
	if x > y {
		return x
	}
	return y
}

func Min(x, y float64) float64 {
	if x < y {
		return x
	}
	return y
}

// Round returns the nearest integer, rounding half away from zero.
func Round(x float64) float64 {
	if x < 0 {
		return ceil(x - 0.5)
	}
	return floor(x + 0.5)
}

func Hypot(p, q float64) float64 { return sqrt(p*p + q*q) }
