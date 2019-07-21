class Triangle {

    static boolean isEquilateral(Number a, Number b, Number c) {
        isTriangle(a, b, c) && (a == b && a == c)
    }

    static boolean isIsosceles(Number a, Number b, Number c) {
        isTriangle(a, b, c) && (a == b || a == c || b == c)
    }

    static boolean isScalene(Number a, Number b, Number c) {
        isTriangle(a, b, c) && (a != b && a != c && b != c)
    }

    static boolean isTriangle(Number a, Number b, Number c) {
        def (x, y, z) = [a, b, c].sort(false)
        return (x > 0) && (x + y > z)
    }
}
