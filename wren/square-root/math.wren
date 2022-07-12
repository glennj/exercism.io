class Math {

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
}
