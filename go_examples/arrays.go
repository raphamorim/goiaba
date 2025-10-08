package main

// Array manipulation examples demonstrating various operations

//export array_sum
func array_sum() int {
    numbers := []int{10, 20, 30, 40, 50}
    total := 0
    for i := 0; i < 5; i++ {
        total = total + numbers[i]
    }
    return total
}

//export find_max
func find_max() int {
    arr := []int{3, 7, 2, 9, 1, 12, 5}
    max := arr[0]
    for i := 1; i < 7; i++ {
        if arr[i] > max {
            max = arr[i]
        }
    }
    return max
}

//export find_min
func find_min() int {
    arr := []int{42, 17, 99, 3, 56}
    min := arr[0]
    for i := 1; i < 5; i++ {
        if arr[i] < min {
            min = arr[i]
        }
    }
    return min
}

//export reverse_array
func reverse_array() int {
    arr := []int{1, 2, 3, 4, 5}
    left := 0
    right := 4
    
    for left < right {
        temp := arr[left]
        arr[left] = arr[right]
        arr[right] = temp
        left++
        right--
    }
    
    // Return first element after reversal (should be 5)
    return arr[0]
}

//export linear_search
func linear_search(target int) int {
    arr := []int{10, 25, 30, 45, 50, 75, 80}
    for i := 0; i < 7; i++ {
        if arr[i] == target {
            return i
        }
    }
    return -1
}

//export count_even
func count_even() int {
    arr := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
    count := 0
    for i := 0; i < 10; i++ {
        if arr[i] % 2 == 0 {
            count++
        }
    }
    return count
}

//export bubble_sort_single_pass
func bubble_sort_single_pass() int {
    arr := []int{5, 2, 8, 1, 9}
    
    // Single pass of bubble sort
    for i := 0; i < 4; i++ {
        if arr[i] > arr[i+1] {
            temp := arr[i]
            arr[i] = arr[i+1]
            arr[i+1] = temp
        }
    }
    
    // Return first element
    return arr[0]
}

//export array_product
func array_product() int {
    arr := []int{2, 3, 4, 5}
    product := 1
    for i := 0; i < 4; i++ {
        product = product * arr[i]
    }
    return product
}

//export count_positive
func count_positive() int {
    arr := []int{-1, 5, -3, 7, 0, 2, -8}
    count := 0
    for i := 0; i < 7; i++ {
        if arr[i] > 0 {
            count++
        }
    }
    return count
}

//export array_average
func array_average() int {
    arr := []int{10, 20, 30, 40, 50}
    sum := 0
    for i := 0; i < 5; i++ {
        sum = sum + arr[i]
    }
    return sum / 5
}
