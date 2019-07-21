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


/* delicious
 * https://exercism.io/tracks/groovy/exercises/difference-of-squares/solutions/e8b9e58bb7d641c990363c09ba63b12b

class Squares {
    Integer integer
    public Squares(Integer integer) { this.integer = integer }
    Integer squareOfSums() { sequence().sum().power(2) }
    Integer sumOfSquares() { sequence()*.power(2).sum() }
    Integer difference() { squareOfSums() - sumOfSquares() }
    private Range sequence() { (0..this.integer) }
}


*/
