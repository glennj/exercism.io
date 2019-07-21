/* Iterating up from 2 and testing primality of each number
 * can be exceedingly slow. Here, we will _approximate_ the
 * nth prime number, then use that as an upper bound for
 * the sieve of Eratonsthenes to find the actuan nth prime.
 */

class NthPrime {

    static nth(int n) {
        if (n <= 0)
            throw new ArithmeticException()

        def bound = approximateNthPrime(n)
        def primes = Eratosthenes.sieve(bound)
        primes[n - 1]
    }

    /* Approximate the nth prime number
     * ref: https://exercism.io/tracks/python/exercises/nth-prime/solutions/2a84e7330fab4b4cb04d92cf0f38164d 
     * ref: https://en.wikipedia.org/wiki/Prime_number_theorem
     */
    private static int approximateNthPrime(int n) {
        2 + Math.floor(1.2 * n * Math.log(n))
    }
}

class Eratosthenes  {
    static List<Integer> sieve(int limit) {
        List<Boolean> isPrime = (0..limit).collect {true}
        isPrime[0] = false
        isPrime[1] = false
        for (def i = 2; i * i <= limit; i++) {
            if (isPrime[i]) {
                def step = i == 2 ? 2 : 2 * i
                for (def j = i * i; j <= limit; j += step) {
                    isPrime[j] = false
                }
            }
        }
        isPrime.withIndex()
               .findAll {it[0]}
               .collect {it[1]}
    }
}

