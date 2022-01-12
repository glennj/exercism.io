object CollatzCalculator {
    // define an Int property
    val Int.odd: Boolean get() = this and 1 == 1

    tailrec fun computeStepCount(start: Int, steps: Int = 0): Int {
        require(start > 0)
        return when {
            start == 1 -> steps
            start.odd  -> computeStepCount(3 * start + 1, steps + 1)
            else       -> computeStepCount(start / 2, steps + 1)
        }
    }
}
