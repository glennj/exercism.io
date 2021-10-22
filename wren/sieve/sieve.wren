// The key to making this fast is to limit the number of loop iterations.

class Sieve {
  construct new(limit) {
    _limit = limit
  }

  primes {
    // only find the primes onece
    if (_primes == null) {
      if (_limit < 2) {
        _primes = []
      } else {
        _candidates = List.filled(_limit+1, true)
        // 2 is the first prime
        markMultiples_(2)
        // 3 is the second prime: this and all subsequent primes are odd numbers
        var prime = 3
        while (prime <= _limit) {
          if (_candidates[prime]) markMultiples_(prime)
          prime = prime + 2
        }
        // and collect the prime numbers
        _primes = (2.._limit).where {|i| _candidates[i]}.toList
      }
    }
    return _primes
  }

  markMultiples_(prime) {
    // all multiples `prime * k` where k < prime have already been marked as non-prime
    var idx = prime * prime
    // for prime > 2, all even multiples are already non-prime
    var step = (prime == 2) ? 2 : 2 * prime
    while (idx <= _limit) {
      _candidates[idx] = false
      idx = idx + step
    }
  }
}
