class PrimeFactors {
  List<int> factors(int number) {
    var fs = <int>[];
    var factor = 2;

    while (factor * factor <= number) {
      if (number % factor == 0) {
        fs.add(factor);
        number ~/= factor;
      } else {
        factor++;
      }
    }

    if (number > 1) fs.add(number);
    return fs;
  }
}
