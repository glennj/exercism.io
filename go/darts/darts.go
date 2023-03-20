package darts

//import "math"

func Score(x, y float64) int {
    /*
       dist := math.Hypot(x, y)
       switch {
           case dist <=  1: return 10
           case dist <=  5: return 5
           case dist <= 10: return 1
           default:         return 0
       }
    */
    d2 := x*x + y*y
    switch {
        case d2 <=   1: return 10
        case d2 <=  25: return 5
        case d2 <= 100: return 1
        default:        return 0
    }
}

/* benchmarks
 *
 * using the math module
 * BenchmarkScore-2        12742710                84.87 ns/op            0 B/op          0 allocs/op
 *
 * not using math
 * BenchmarkScore-2        144869790                8.148 ns/op           0 B/op          0 allocs/op
 */
