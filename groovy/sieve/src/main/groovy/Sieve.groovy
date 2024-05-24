class Sieve {
    static primes(Integer limit) {
        def flags = (0..limit).collect { true }
        flags[0] = false
        flags[1] = false

        def markMultiples = { p, step ->
            for (def m = p * p; m <= limit; m += step)
                flags[m] = false
        }

        markMultiples(2, 2)
        for (def p = 3; p * p <= limit; p += 2)
            if (flags[p])
                markMultiples(p, p * 2)

        flags.withIndex()
             .findAll { isPrime, p -> isPrime }
             .collect { isPrime, p -> p }
    }

}
