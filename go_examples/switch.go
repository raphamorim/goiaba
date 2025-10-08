package main

// Switch statement examples for control flow

//export day_of_week
func day_of_week(day int) int {
    switch day {
    case 1:
        return 100 // Monday
    case 2:
        return 200 // Tuesday
    case 3:
        return 300 // Wednesday
    case 4:
        return 400 // Thursday
    case 5:
        return 500 // Friday
    case 6:
        return 600 // Saturday
    case 7:
        return 700 // Sunday
    default:
        return 0
    }
}

//export is_weekday
func is_weekday(day int) int {
    switch day {
    case 1, 2, 3, 4, 5:
        return 1
    case 6, 7:
        return 0
    default:
        return -1
    }
}

//export grade_to_gpa
func grade_to_gpa(grade int) int {
    switch grade {
    case 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100:
        return 4
    case 80, 81, 82, 83, 84, 85, 86, 87, 88, 89:
        return 3
    case 70, 71, 72, 73, 74, 75, 76, 77, 78, 79:
        return 2
    case 60, 61, 62, 63, 64, 65, 66, 67, 68, 69:
        return 1
    default:
        return 0
    }
}

//export season_from_month
func season_from_month(month int) int {
    switch month {
    case 12, 1, 2:
        return 1 // Winter
    case 3, 4, 5:
        return 2 // Spring
    case 6, 7, 8:
        return 3 // Summer
    case 9, 10, 11:
        return 4 // Fall
    default:
        return 0
    }
}

//export calculator_switch
func calculator_switch(op int, a int, b int) int {
    switch op {
    case 1:
        return a + b
    case 2:
        return a - b
    case 3:
        return a * b
    case 4:
        if b != 0 {
            return a / b
        }
        return 0
    default:
        return 0
    }
}

//export http_status_category
func http_status_category(status int) int {
    switch status {
    case 200, 201, 204:
        return 1 // Success
    case 400, 401, 403, 404:
        return 2 // Client error
    case 500, 502, 503:
        return 3 // Server error
    default:
        return 0
    }
}

//export color_code
func color_code(color int) int {
    switch color {
    case 1:
        return 255 // Red
    case 2:
        return 65280 // Green
    case 3:
        return 255 // Blue
    default:
        return 0
    }
}

//export priority_level
func priority_level(urgency int, importance int) int {
    switch urgency {
    case 1:
        switch importance {
        case 1:
            return 4 // Critical
        case 2:
            return 3 // High
        default:
            return 2
        }
    case 2:
        switch importance {
        case 1:
            return 3
        case 2:
            return 2
        default:
            return 1
        }
    default:
        return 1 // Low
    }
}

//export traffic_light
func traffic_light(signal int) int {
    switch signal {
    case 1:
        return 10 // Red - stop
    case 2:
        return 20 // Yellow - slow
    case 3:
        return 30 // Green - go
    default:
        return 0
    }
}

//export month_days
func month_days(month int) int {
    switch month {
    case 1, 3, 5, 7, 8, 10, 12:
        return 31
    case 4, 6, 9, 11:
        return 30
    case 2:
        return 28
    default:
        return 0
    }
}
