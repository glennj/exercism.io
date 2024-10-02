import 'range.dart';

extension IntUpTo on int {
  Range upto(int n) => Range(this, n);
}

extension IntPow on int {
  int pow(int exponent) {
    if (exponent.isNegative)
      throw ArgumentError('The exponent must be non-negative.');

    int result = 1;
    for (var i = 1; i <= exponent; i++) result *= this;
    return result;
  }
}
