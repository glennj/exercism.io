class SaddlePoints {

    static getSaddlePoints(matrix) {
        def rowMax = matrix.collect { it.max() }
        def colMin = matrix.transpose().collect { it.min() }

        def saddlePoints = []
        matrix.eachWithIndex { row, x ->
            row.eachWithIndex { elem, y ->
                if (elem == rowMax[x] && elem == colMin[y])
                    saddlePoints << [x, y]
            }
        }
        saddlePoints
    }
}
