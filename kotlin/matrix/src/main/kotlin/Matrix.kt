class Matrix(private val matrixAsString: String) {

    // parse the input string at object creation time.
    private val mtx: List<List<Int>>
            = matrixAsString
                .split('\n')
                .map { line ->
                    Regex("\\d+")                   // digit sequences
                        .findAll(line)              // in the line
                        .map { it.value.toInt() }   // as integers
                        .toList()                   // Sequence => List
                }

    fun row(rowNr: Int): List<Int> = mtx[rowNr - 1]

    fun column(colNr: Int): List<Int> = mtx.map { it[colNr - 1] }
}
