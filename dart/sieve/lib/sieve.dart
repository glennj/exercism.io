class Sieve {
  final int limit;
  final List<int> _primes = [];
  Sieve(this.limit);

  List<int> get primes {
    if (limit >= 2 && _primes.isEmpty) {
      var marks = List.generate(limit + 1, (i) => i);
      marks[1] = 0;

      void markMultiples(int n, {int step = 2}) {
        for (var i = n * n; i <= limit; i += step) {
          marks[i] = 0;
        }
      }

      markMultiples(2);
      for (int prime = 3; prime * prime <= limit; prime += 2) {
        if (marks[prime] != 0) {
          markMultiples(prime, step: 2 * prime);
        }
      }

      _primes.addAll(marks.where((i) => i != 0));
    }
    return _primes;
  }
}
