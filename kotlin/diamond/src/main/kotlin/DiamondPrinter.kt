class DiamondPrinter {

    private fun row(n: Int, i: Int): String =
        with( " ".repeat(i) + ('A' + i).toString() + " ".repeat(n - i - 1)) {
            this.drop(1).reversed() + this
        }

    fun printToList(c: Char): List<String> =
        (c - 'A' + 1).let { n ->
            require(c in 'A'..'Z')
            with(
                (0 until n).map { row(n, it) }
            ) {
                this + this.reversed().drop(1)
            }
        }
}
