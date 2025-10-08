package main

// Advanced algorithms and problem-solving examples

//export is_prime
func is_prime(n int) int {
    if n <= 1 {
        return 0
    }
    if n <= 3 {
        return 1
    }
    if n%2 == 0 || n%3 == 0 {
        return 0
    }
    
    i := 5
    for i*i <= n {
        if n%i == 0 || n%(i+2) == 0 {
            return 0
        }
        i = i + 6
    }
    return 1
}

//export gcd
func gcd(a int, b int) int {
    for b != 0 {
        temp := b
        b = a % b
        a = temp
    }
    return a
}

//export lcm
func lcm(a int, b int) int {
    if a == 0 || b == 0 {
        return 0
    }
    return (a * b) / gcd(a, b)
}

//export is_palindrome_number
func is_palindrome_number(n int) int {
    if n < 0 {
        return 0
    }
    
    original := n
    reversed := 0
    
    for n > 0 {
        digit := n % 10
        reversed = reversed*10 + digit
        n = n / 10
    }
    
    if original == reversed {
        return 1
    }
    return 0
}

//export sum_of_digits
func sum_of_digits(n int) int {
    sum := 0
    if n < 0 {
        n = -n
    }
    
    for n > 0 {
        sum = sum + (n % 10)
        n = n / 10
    }
    return sum
}

//export count_digits
func count_digits(n int) int {
    if n == 0 {
        return 1
    }
    
    count := 0
    if n < 0 {
        n = -n
    }
    
    for n > 0 {
        count++
        n = n / 10
    }
    return count
}

//export reverse_number
func reverse_number(n int) int {
    reversed := 0
    negative := 0
    
    if n < 0 {
        negative = 1
        n = -n
    }
    
    for n > 0 {
        digit := n % 10
        reversed = reversed*10 + digit
        n = n / 10
    }
    
    if negative == 1 {
        reversed = -reversed
    }
    
    return reversed
}

//export binary_search
func binary_search(target int) int {
    arr := []int{1, 3, 5, 7, 9, 11, 13, 15, 17, 19}
    left := 0
    right := 9
    
    for left <= right {
        mid := (left + right) / 2
        
        if arr[mid] == target {
            return mid
        } else if arr[mid] < target {
            left = mid + 1
        } else {
            right = mid - 1
        }
    }
    
    return -1
}

//export nth_triangular
func nth_triangular(n int) int {
    return (n * (n + 1)) / 2
}

//export collatz_steps
func collatz_steps(n int) int {
    steps := 0
    
    for n != 1 {
        if n%2 == 0 {
            n = n / 2
        } else {
            n = 3*n + 1
        }
        steps++
        
        if steps > 1000 {
            return -1
        }
    }
    
    return steps
}

//export perfect_number_check
func perfect_number_check(n int) int {
    if n <= 0 {
        return 0
    }
    
    sum := 0
    for i := 1; i < n; i++ {
        if n%i == 0 {
            sum = sum + i
        }
    }
    
    if sum == n {
        return 1
    }
    return 0
}

//export sum_of_squares
func sum_of_squares(n int) int {
    sum := 0
    for i := 1; i <= n; i++ {
        sum = sum + (i * i)
    }
    return sum
}
