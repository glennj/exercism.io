class NthPrime {
  static prime(number) {
    if (number < 1) Fiber.abort("there is no zeroth prime")

    var png = primeNumberGenerator()
    var prime = 0
    for (i in 1..number) prime = png.call()
    return prime
  }

  static primeNumberGenerator() {
    return Fiber.new {
      var primes = []

      var isPrime = Fn.new { |n|
        if (n <= primes[-1]) return primes.contains(n)
        for (p in primes) {
          if (n % p == 0) return false
          if (p * p > n) break        // we can stop when p > sqrt(n)
        }
        return true
      }

      // start generating primes
      var prime = 2
      Fiber.yield(prime)
      primes.add(prime)

      prime = 3
      while (true) {
        Fiber.yield(prime)
        primes.add(prime)
        prime = prime + 2
        while (!isPrime.call(prime)) prime = prime + 2
      }
    }
  }
}
