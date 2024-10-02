import 'dart:math';

/*
Using the Binary numeral system (base 2) from Wikipedia
https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_%28base_2%29
*/

class SquareRoot {
  int squareRoot(int radicand) {
    // find `b`, the greatest power of 4 ≤ radicand
    var b = 4.pow( (log(radicand) / log(4)).floor() );
    var root = 0;
    while (b > 0) {
      if (radicand >= root + b) {
        radicand -= root + b;
        root = root ~/ 2 + b;
      }
      else {
        root ~/= 2;
      }
      b ~/= 4;
    }
    return root;
  }
}

extension IntMethods on int {
  int pow(int exponent) {
    var result = 1;
    for (var i = 1; i <= exponent; i++) result *= this;
    return result;
  }
}
