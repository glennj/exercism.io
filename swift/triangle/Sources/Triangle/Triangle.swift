
struct Triangle {
    private(set) var kind: String

    init(_ a: Double, _ b: Double, _ c: Double) {
        let sides = [a, b, c].sorted()
        if sides[0] <= 0 || sides[0] + sides[1] <= sides[2] {
            kind = "ErrorKind"
        } else {
            let s: Set = [a, b, c]
            switch s.count {
                case 1:  kind = "Equilateral"
                case 2:  kind = "Isosceles"
                default: kind = "Scalene"
            }
        }
    }
}
