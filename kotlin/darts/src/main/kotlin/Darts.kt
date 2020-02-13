object Darts {

    fun score(x: Number, y: Number): Int {
        val distance = kotlin.math.hypot(x.toDouble(), y.toDouble())

        return when {
            distance <= 1  -> 10
            distance <= 5  -> 5
            distance <= 10 -> 1
            else           -> 0
        }
    }
}
