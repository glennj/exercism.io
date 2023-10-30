data class MatrixCoordinate(val row: Int, val col: Int)

class Matrix(private val m: List<List<Int>>) {

    val saddlePoints: Set<MatrixCoordinate> = findSaddlePoints()

    private fun findSaddlePoints(): Set<MatrixCoordinate> {
        val saddlePoints: MutableSet<MatrixCoordinate> = mutableSetOf()
        if (m.isEmpty() || m.first().isEmpty()) {
            return saddlePoints
        }

        val rowMaximum = m.map { it.maxOrNull()!! }
        val colMinimum = m.first().indices
                .map { i -> m.map { row -> row[i] } }
                .map { it.minOrNull()!! }

        // check each cell for its saddle-pointiness
        for (r in m.indices) {
            for (c in m[r].indices) {
                if (m[r][c] == rowMaximum[r] && m[r][c] == colMinimum[c]) {
                    saddlePoints += MatrixCoordinate(r + 1,c + 1)
                }
            }
        }
        return saddlePoints
    }
}
