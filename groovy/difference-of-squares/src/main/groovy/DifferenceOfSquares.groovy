class DifferenceOfSquares {

    final num

    DifferenceOfSquares(num) {
        this.num = num
    }

    def squareOfSum() {
        /*
        def sum = 0
        1.upto(this.num) {sum += it}
        sum ** 2
        */
        new IntRange(1, num).sum() ** 2
    }

    def sumOfSquares() {
        /*
        def sum = 0
        1.upto(this.num) {sum += it ** 2}
        sum
        */
        new IntRange(1, num).collect {it ** 2}.sum()
    }

    def difference() {
        squareOfSum() - sumOfSquares()
    }

}
