package main

// String manipulation and operations

//export string_length
func string_length() int {
    s := "Hello, World!"
    return len(s)
}

//export empty_string_check
func empty_string_check() int {
    s := ""
    if len(s) == 0 {
        return 1
    }
    return 0
}

//export compare_lengths
func compare_lengths() int {
    s1 := "Go"
    s2 := "WebAssembly"
    
    len1 := len(s1)
    len2 := len(s2)
    
    if len1 > len2 {
        return 1
    } else if len1 < len2 {
        return -1
    } else {
        return 0
    }
}

//export total_chars
func total_chars() int {
    s1 := "Hello"
    s2 := "World"
    s3 := "!"
    
    return len(s1) + len(s2) + len(s3)
}

//export is_long_string
func is_long_string() int {
    s := "This is a long string for testing"
    if len(s) > 20 {
        return 1
    }
    return 0
}

//export count_all_chars
func count_all_chars() int {
    messages := []int{
        len("first"),
        len("second"),
        len("third"),
        len("fourth"),
    }
    
    total := 0
    for i := 0; i < 4; i++ {
        total = total + messages[i]
    }
    return total
}

//export validate_input
func validate_input() int {
    input := "test123"
    minLength := 5
    maxLength := 10
    
    length := len(input)
    
    if length >= minLength && length <= maxLength {
        return 1
    }
    return 0
}

//export escape_sequences
func escape_sequences() int {
    // Test escape sequence handling
    s := "line1\nline2\ttab"
    return len(s)
}

//export string_with_numbers
func string_with_numbers() int {
    s := "12345678910"
    return len(s)
}

//export max_string_length
func max_string_length() int {
    s1 := "short"
    s2 := "medium length"
    s3 := "very long string here"
    
    max := len(s1)
    
    if len(s2) > max {
        max = len(s2)
    }
    if len(s3) > max {
        max = len(s3)
    }
    
    return max
}
