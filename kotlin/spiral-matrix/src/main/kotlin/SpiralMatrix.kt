object SpiralMatrix {

    /*
    * If we were using a 2-D array with, say, n=3
    *
    * 0 1 2       1 2 3
    * 3 4 5   =>  8 9 4
    * 6 7 8       7 6 5
    *
    * Flattening that into a 1-D array:
    *
    * idx: 0 1 2 3 4 5 6 7 8
    * val: 1 2 3 8 9 4 7 6 5

    * With the idx initialized to -1, for a length of 3, we
    * increment idx by 1; then for a length of 2, we increment
    * by 3; then for a length of 2 we increment by -1; then
    * for a length of 1 we increment by -3; then for a length of 1
    * we increment by 1.
    *
    * For the length delta, we have a pattern of
    * (subtract 1, subtract 0) and repeat.
    *
    * For the index delta, we have a pattern of
    * (add 1, add N, subtract 1, subtract N) and repeat.
    */

    private fun lenCycle(): Iterator<Int> =
            generateSequence(1) { if (it == 1) 0 else 1 }.iterator()

    private fun idxCycle(size: Int): Iterator<Int> =
            generateSequence(1) {
                when(it) {
                    1 -> size
                    size -> -1
                    -1 -> -size
                    else -> 1
                }
            }.iterator()

    fun ofSize(size: Int): Array<IntArray> {
        val oneDim = IntArray(size * size)
        val lenIter = lenCycle()
        val idxIter = idxCycle(size)
        var n = size
        var idx = -1
        var value = 1

        while (value <= size * size) {
            val idxDelta = idxIter.next()
            repeat(n) {
                idx += idxDelta
                oneDim[idx] = value++
            }
            n -= lenIter.next()
        }

        return (0 until size).map {
            oneDim.slice(it * size until (it + 1) * size).toIntArray()
        }.toTypedArray()
    }
}
