###
Using the Binary numeral system (base 2) from Wikipedia
https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_%28base_2%29
###

class SquareRoot
  @squareRoot: (n) ->
    # find `b`, the greatest power of 4 ≤ n
    b = 4 ** Math.floor(Math.log(n) / Math.log(4))
    root = 0
    while b > 0
      if n >= root + b
        n -= root + b
        root = root // 2 + b
      else
        root //= 2
      b //= 4
    root

module.exports = SquareRoot
