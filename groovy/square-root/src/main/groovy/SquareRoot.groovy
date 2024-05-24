/*
 * Using the Binary numeral system (base 2) from Wikipedia
 * https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_%28base_2%29
 */

class SquareRoot {
    static Integer squareRoot(Integer radicand) {
        // b: the greatest power of 4 less than or equal to radicand
        int b = 4 ** Math.floor(Math.log(radicand) / Math.log(4))
        int x = 0

        while (b > 0) {
            if (radicand >= x + b) {
                radicand -= (x + b)
                x = x / 2 + b
            } else {
                x = x / 2
            }
            b /= 4
        }
        return x
    }
}
