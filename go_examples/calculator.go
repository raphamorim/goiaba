package main

//export add
func add(x int, y int) int {
    return x + y
}

//export multiply  
func multiply(a int, b int) int {
    return a * b
}

//export factorial
func factorial(n int) int {
    if n <= 1 {
        return 1
    }
    return n * factorial(n-1)
}

//export max
func max(a int, b int) int {
    if a >= b {
        return a
    } else {
        return b
    }
}

//export abs
func abs(x int) int {
    if x < 0 {
        return -x
    }
    return x
}

//export sign
func sign(x int) int {
    if x > 0 {
        return 1
    } else if x < 0 {
        return -1
    } else {
        return 0
    }
}

//export sum_to_n
func sum_to_n(n int) int {
    sum := 0
    for i := 0; i < n; i++ {
        sum = sum + i
    }
    return sum
}

//export fibonacci
func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    return fibonacci(n-1) + fibonacci(n-2)
}

//export power
func power(base int, exp int) int {
    if exp == 0 {
        return 1
    }
    if exp == 1 {
        return base
    }
    
    result := 1
    for i := 0; i < exp; i++ {
        result = result * base
    }
    return result
}

//export bitwise_and
func bitwise_and(a int, b int) int {
    return a & b
}

//export bitwise_or
func bitwise_or(a int, b int) int {
    return a | b
}

//export bitwise_xor
func bitwise_xor(a int, b int) int {
    return a ^ b
}

//export left_shift
func left_shift(a int, b int) int {
    return a << b
}

//export right_shift
func right_shift(a int, b int) int {
    return a >> b
}