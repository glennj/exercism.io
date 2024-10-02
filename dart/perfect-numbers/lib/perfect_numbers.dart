import 'dart:math';

enum Classification {
  perfect,
  abundant,
  deficient,
}

class PerfectNumbers {
  Classification classify(int n) {
    if (n <= 0) throw ArgumentError('number must be positive');
    
    var sum = _aliquotSum(n);
    if (sum < n) return Classification.deficient;
    if (sum > n) return Classification.abundant;
    return Classification.perfect;
  }

  int _aliquotSum(int n) {
    int sum = 0;
    for (var f = sqrt(n).floor(); f >= 1; f--) {
      if (n % f == 0) {
        sum += f;
        if (f * f != n) sum += n ~/ f;
      }
    }
    return sum - n;
  }
}
