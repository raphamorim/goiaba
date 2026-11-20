package os

func exit(code int)
func write(fd int, p []byte) int
func nargs() int
func arg(i int, out *string)
func getenv(name string, out *string) bool

// File is an open file descriptor.
type File struct {
	fd int
}

var (
	Stdin  = &File{0}
	Stdout = &File{1}
	Stderr = &File{2}
)

// Args holds the command-line arguments, starting with the program name.
var Args []string

func init() {
	n := nargs()
	for i := 0; i < n; i++ {
		var s string
		arg(i, &s)
		Args = append(Args, s)
	}
}

func (f *File) Write(p []byte) (n int, err error) {
	return write(f.fd, p), nil
}

func (f *File) WriteString(s string) (n int, err error) {
	return write(f.fd, []byte(s)), nil
}

// Exit ends the program with the given status code. Deferred calls do not run.
func Exit(code int) {
	exit(code)
}

// Getenv returns the value of the environment variable, or "" if it is unset.
func Getenv(key string) string {
	var value string
	getenv(key, &value)
	return value
}

// LookupEnv returns the value of the environment variable and whether it is set.
func LookupEnv(key string) (string, bool) {
	var value string
	ok := getenv(key, &value)
	return value, ok
}
