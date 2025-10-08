package main

// Combined examples demonstrating multiple features together

type Stats struct {
    min int
    max int
    sum int
    avg int
}

//export analyze_array
func analyze_array() int {
    numbers := []int{5, 12, 3, 18, 7, 9}
    
    stats := Stats{
        min: numbers[0],
        max: numbers[0],
        sum: 0,
        avg: 0,
    }
    
    for i := 0; i < 6; i++ {
        if numbers[i] < stats.min {
            stats.min = numbers[i]
        }
        if numbers[i] > stats.max {
            stats.max = numbers[i]
        }
        stats.sum = stats.sum + numbers[i]
    }
    
    stats.avg = stats.sum / 6
    
    return stats.min + stats.max + stats.avg
}

//export grade_calculator
func grade_calculator(score int) int {
    switch {
    case score >= 90:
        return 4 // A
    case score >= 80:
        return 3 // B
    case score >= 70:
        return 2 // C
    case score >= 60:
        return 1 // D
    default:
        return 0 // F
    }
}

//export validate_and_process
func validate_and_process(value int) int {
    // Validate input
    if value < 0 {
        return -1
    }
    
    // Process based on range
    switch {
    case value < 10:
        return value * 10
    case value < 100:
        return value * 5
    case value < 1000:
        return value * 2
    default:
        return value
    }
}

//export filter_and_count
func filter_and_count() int {
    data := []int{1, 5, 10, 15, 20, 25, 30, 35, 40}
    count := 0
    
    for i := 0; i < 9; i++ {
        switch {
        case data[i] < 10:
            count = count + 1
        case data[i] >= 20 && data[i] < 30:
            count = count + 2
        case data[i] >= 30:
            count = count + 3
        }
    }
    
    return count
}

//export string_category
func string_category() int {
    s1 := "short"
    s2 := "medium length string"
    s3 := "very long string with many characters"
    
    len1 := len(s1)
    len2 := len(s2)
    len3 := len(s3)
    
    category := 0
    
    if len1 < 10 {
        category = category + 1
    }
    if len2 >= 10 && len2 < 30 {
        category = category + 10
    }
    if len3 >= 30 {
        category = category + 100
    }
    
    return category
}

//export complex_calculation
func complex_calculation(x int, y int, operation int) int {
    result := 0
    
    switch operation {
    case 1:
        // Sum of squares
        result = (x * x) + (y * y)
    case 2:
        // Product of sum and difference
        result = (x + y) * (x - y)
    case 3:
        // Average of range
        if x < y {
            sum := 0
            for i := x; i <= y; i++ {
                sum = sum + i
            }
            result = sum / (y - x + 1)
        }
    default:
        result = x + y
    }
    
    return result
}

//export array_transform
func array_transform(mode int) int {
    arr := []int{1, 2, 3, 4, 5}
    
    switch mode {
    case 1:
        // Double all elements
        for i := 0; i < 5; i++ {
            arr[i] = arr[i] * 2
        }
    case 2:
        // Square all elements
        for i := 0; i < 5; i++ {
            arr[i] = arr[i] * arr[i]
        }
    case 3:
        // Increment all elements
        for i := 0; i < 5; i++ {
            arr[i] = arr[i] + 10
        }
    }
    
    // Return sum
    sum := 0
    for i := 0; i < 5; i++ {
        sum = sum + arr[i]
    }
    return sum
}

//export point_distance_category
func point_distance_category(x1 int, y1 int, x2 int, y2 int) int {
    type Point struct {
        x int
        y int
    }
    
    p1 := Point{x: x1, y: y1}
    p2 := Point{x: x2, y: y2}
    
    dx := p1.x - p2.x
    dy := p1.y - p2.y
    distanceSquared := dx*dx + dy*dy
    
    switch {
    case distanceSquared < 25:
        return 1 // Very close
    case distanceSquared < 100:
        return 2 // Close
    case distanceSquared < 400:
        return 3 // Medium
    default:
        return 4 // Far
    }
}
