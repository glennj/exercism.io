struct Sieve {
    private(set) var primes: [Int] = []

    init(_ limit: Int) {
        self.primes = self.eratosthenes(limit)
    }

    private func eratosthenes(_ limit: Int) -> [Int] {
        var isPrime = Array(repeating: true, count: limit + 1)
        isPrime[0] = false
        isPrime[1] = false
        var result = Array<Int>()

        for i in 2...limit {
            if isPrime[i] {
                // this is a prime number
                result.append(i)

                // mark multiples as non-prime
                var j = i * i
                while j <= limit {
                    isPrime[j] = false
                    j += i * (i == 2 ? 1 : 2)
                }
            }
        }
        return result
    }
}