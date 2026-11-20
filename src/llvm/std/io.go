package io

import "errors"

// Writer is the interface that wraps the basic Write method.
type Writer interface {
	Write(p []byte) (n int, err error)
}

// EOF is the error returned when no more input is available.
var EOF = errors.New("EOF")
