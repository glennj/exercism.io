class PascalsTriangle {

    static rows(count) {
        def triangle = [[1]]

        for (def i = 1; i <= count - 1; i++) {
            def row = [1]
            for (def j = 1; j <= i; j++)
                row.add (triangle[i - 1][j - 1] + (triangle[i - 1][j] ?: 0))
            triangle.add row
        }

        return triangle.take(count)
    }
}
