class Matrix {

    final int[][] rows
    final int[][] cols

    Matrix(String asString) {
        def m = asString.split("\n")
                        .collect { it.split(" ")*.toInteger() }
        rows = m
        cols = m.transpose()
    }

    int[] row(int rowNumber)   { rows[rowNumber] }

    int[] column(columnNumber) { cols[columnNumber] }
}
