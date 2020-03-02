data class MinesweeperBoard(val board: List<String>) {

    fun withNumbers(): List<String> =
            with (board.toGrid()) {
                this.indices.forEach { i ->
                    this[i].indices.forEach { j ->
                        if (this[i][j].isMine()) this.incrementNeighbours(i, j)
                    }
                }
                this.toBoard()
            }


    private fun List<String>.toGrid(): MutableList<MutableList<Int>> =
            this.map {
                it.map { c ->
                    mapOf(' ' to 0, '*' to -1)[c] ?: error("unexpected character")
                }.toMutableList()
            }.toMutableList()


    private fun Int.isMine(): Boolean = this == -1


    private fun MutableList<MutableList<Int>>.toBoard(): List<String> =
            this.map {
                it.joinToString("") { n ->
                    mapOf(-1 to "*", 0 to " ").getOrDefault(n, n.toString())
                }
            }


    private fun MutableList<MutableList<Int>>.incrementNeighbours(i: Int, j: Int) {
        for (ii in i-1..i+1) {
            for (jj in j-1..j+1) {
                if (    ii in (0 until this.size) &&
                        jj in (0 until this[i].size) &&
                        !this[ii][jj].isMine()
                ) this[ii][jj]++
            }
        }
    }
}
