package errors

type errorString struct {
	s string
}

func (e *errorString) Error() string { return e.s }

// New returns an error that formats as the given text. Each call returns a
// distinct value, even for the same text.
func New(text string) error {
	return &errorString{text}
}

type wrapper interface {
	Unwrap() error
}

// Unwrap returns the error err wraps, or nil.
func Unwrap(err error) error {
	w, ok := err.(wrapper)
	if !ok {
		return nil
	}
	return w.Unwrap()
}

// Is reports whether err or any error it wraps is target.
func Is(err, target error) bool {
	for err != nil {
		if err == target {
			return true
		}
		err = Unwrap(err)
	}
	return false
}
