package main

// Struct examples demonstrating data structures

type Point struct {
    x int
    y int
}

type Rectangle struct {
    width  int
    height int
}

type Circle struct {
    radius int
}

type Person struct {
    age    int
    height int
}

//export create_point
func create_point(x int, y int) int {
    p := Point{x: x, y: y}
    return p.x + p.y
}

//export point_distance_squared
func point_distance_squared(x1 int, y1 int, x2 int, y2 int) int {
    p1 := Point{x: x1, y: y1}
    p2 := Point{x: x2, y: y2}
    
    dx := p1.x - p2.x
    dy := p1.y - p2.y
    
    return dx*dx + dy*dy
}

//export rectangle_area
func rectangle_area(width int, height int) int {
    rect := Rectangle{
        width:  width,
        height: height,
    }
    return rect.width * rect.height
}

//export rectangle_perimeter
func rectangle_perimeter(width int, height int) int {
    rect := Rectangle{width: width, height: height}
    return 2 * (rect.width + rect.height)
}

//export circle_area_approx
func circle_area_approx(radius int) int {
    c := Circle{radius: radius}
    // Approximate pi as 3 for integer math
    return 3 * c.radius * c.radius
}

//export update_point
func update_point(x int, y int, dx int, dy int) int {
    p := Point{x: x, y: y}
    p.x = p.x + dx
    p.y = p.y + dy
    return p.x + p.y
}

//export compare_rectangles
func compare_rectangles(w1 int, h1 int, w2 int, h2 int) int {
    rect1 := Rectangle{width: w1, height: h1}
    rect2 := Rectangle{width: w2, height: h2}
    
    area1 := rect1.width * rect1.height
    area2 := rect2.width * rect2.height
    
    if area1 > area2 {
        return 1
    } else if area1 < area2 {
        return -1
    } else {
        return 0
    }
}

//export point_quadrant
func point_quadrant(x int, y int) int {
    p := Point{x: x, y: y}
    
    if p.x > 0 && p.y > 0 {
        return 1
    } else if p.x < 0 && p.y > 0 {
        return 2
    } else if p.x < 0 && p.y < 0 {
        return 3
    } else if p.x > 0 && p.y < 0 {
        return 4
    }
    return 0
}

//export scale_rectangle
func scale_rectangle(w int, h int, factor int) int {
    rect := Rectangle{width: w, height: h}
    rect.width = rect.width * factor
    rect.height = rect.height * factor
    return rect.width + rect.height
}

//export person_is_adult
func person_is_adult(age int) int {
    p := Person{age: age, height: 170}
    if p.age >= 18 {
        return 1
    }
    return 0
}
