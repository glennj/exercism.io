class Triangle<out T : Number>(val a: T, val b: T, val c: T) {

    init {
        val sides = listOf(a, b, c).map(Number::toDouble).sorted()
        require(sides[0] > 0 && sides[0] + sides[1] > sides[2]) {
            "not a triangle"
        }
    }

    private val numSides = setOf(a, b, c).size

    val isEquilateral: Boolean = numSides == 1
    val isIsosceles:   Boolean = numSides <= 2
    val isScalene:     Boolean = numSides == 3
}
