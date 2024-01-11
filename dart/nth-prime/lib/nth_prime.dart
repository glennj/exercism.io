class NthPrime {
  int prime(int nth) {
    if (nth < 1) throw ArgumentError('There is no zeroth prime');
    var n = 0;
    var p = -1;
    for (p in nextPrime()) {
      if (++n == nth) break;
    }
    return p;
  }

  var _primes = <int>[];

  Iterable<int> nextPrime() sync* {
    _primes.add(2);
    yield 2;
    var p = 3; 
    while (true) {
      _primes.add(p);
      yield p;
      while (true) {
        p += 2;
        if (isPrime(p)) break;
      }
    }
  }

  bool isPrime(int n) {
    for (var p in _primes) {
      if (p * p > n) break;
      if (n % p == 0) return false;
    }
    return true;
  }
}
