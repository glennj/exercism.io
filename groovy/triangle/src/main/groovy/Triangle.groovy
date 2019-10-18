class Triangle {

    Number a, b, c

    Triangle(Number x, Number y, Number z) {
        (a, b, c) = [x, y, z]
    }

    boolean isEquilateral() {
        isTriangle() && (a == b && a == c)
    }

    boolean isIsosceles() {
        isTriangle() && (a == b || a == c || b == c)
    }

    boolean isScalene() {
        isTriangle() && (a != b && a != c && b != c)
    }

    boolean isTriangle() {
        def (x, y, z) = [a, b, c].sort(false)
        return (x > 0) && (x + y > z)
    }
}
