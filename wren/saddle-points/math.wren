/* A library of math function used by various exercises */

class Math {

  /* Greatest Common Demoniator
   */
  static gcd(a, b) { b == 0 ? a : gcd(b, a % b) }

  /* Using the Binary numeral system implementation from
   * https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_(base_2)
   */
  static sqrt(n) {
    // find b, the largest power of 4 <= n
    var b = 4.pow((n.log / 4.log).floor)
    var x = 0

    while (b != 0) {
      if (n >= x + b) {
        n = n - x - b
        x = x / 2 + b
      } else {
        x = x / 2
      }
      b = (b / 4).floor
    }
    return x
  }

  /* Modular Multiplicative Inverse
   * find `n` where `(a * n) mod m == 1`
   */
  static mmi(a, m) {
    var n = 0
    while (n < m) {
      if (a * n % m == 1) {
        return n
      }
      n = n + 1
    }
  }

  static min(a, b) { a < b ? a : b }
  static max(a, b) { a < b ? b : a }
}
