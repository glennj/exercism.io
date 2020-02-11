data class MatrixCoordinate(val row: Int, val col: Int)

class Matrix(private val m: List<List<Int>>) {

    val saddlePoints: Set<MatrixCoordinate> = findSaddlePoints()

    private fun findSaddlePoints(): Set<MatrixCoordinate> {
        val saddlePoints: MutableSet<MatrixCoordinate> = mutableSetOf()
        val rowMaximum = m.map { it.max() }
        val colMinimum = m.first().indices
                .map { i -> m.map { row -> row[i] } }
                .map { it.min() }

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

/*
 * community solutions
 *
 * https://exercism.io/tracks/kotlin/exercises/saddle-points/solutions/bf554a250dd84b648bf1ba045d4daf55
 *
class Matrix(private val rows: List<List<Int>>) {
    private val columns by lazy { (0 until rows[0].size).map { i -> rows.map { it[i] } } }
    private val maxInRow by lazy { rows.map { it.max() } }
    private val minInCol by lazy { columns.map { it.min() } }

    val saddlePoints: Set<MatrixCoordinate> =
            rows.mapIndexed { x, row ->
                row.mapIndexedNotNull { y, it ->
                    if (it == maxInRow[x] && it == minInCol[y]) {
                        MatrixCoordinate(x, y)
                    } else null
                }
            }.flatten().toSet()
}

 *
 * https://exercism.io/tracks/kotlin/exercises/saddle-points/solutions/b426f00692ea43438f9c8f45be719911
 *
class Matrix(private val grid: List<List<Int>>) {
    private val columnMinimums = (0 until grid[0].size).map { col -> grid.map { it[col] }.min() }

    val saddlePoints = grid.foldIndexed(setOf<MatrixCoordinate>(), { rowNumber, coords, row ->
        row.foldIndexed(coords, { columnNumber, acc, i ->
            if (row.max() == i && columnMinimums[columnNumber] == i) acc.plus(MatrixCoordinate(rowNumber, columnNumber)) else acc })
    })
}

 *
 * https://exercism.io/tracks/kotlin/exercises/saddle-points/solutions/47515557f02d4887871b2372a478879d
 *
typealias Coordinate = MatrixCoordinate
typealias IntMatrix = List<List<Int>>

fun IntMatrix.Coordinates() = this.mapIndexed{r, row -> row.mapIndexed{c, _ -> Coordinate(r, c)}}.flatMap{it}

class Matrix(val matrix: IntMatrix) {
    val saddlePoints: Set<Coordinate>
        get() = matrix.Coordinates().filter { (checkRow(it) && checkCol(it)) }.toSet()

    fun checkRow(c: Coordinate) = matrix[c.row][c.col] == matrix[c.row].max()
    fun checkCol(c: Coordinate) = matrix[c.row][c.col] == matrix.map { it[c.col] }.min()
}

 *
 */
