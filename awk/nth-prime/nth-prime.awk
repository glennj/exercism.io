#!/usr/bin/env gawk -f

BEGIN {
    # variable `n` comes from the command line

    if (n <= 0) {
        print "invalid input" > "/dev/stderr"
        exit 1
    }

    # initialize the primes array ("prime the pump", as it were)
    primes[++i] = 2
    primes[++i] = prime = 3

    while (i < n)
        primes[++i] = prime = nextPrime(prime)
    
    print primes[n]
}

function nextPrime(prime,    candidate) {
    candidate = prime + 2
    while (!isPrime(candidate))
        candidate += 2
    return candidate
}

function isPrime(candidate,    i) {
    i = 1
    while (primes[i] <= sqrt(candidate)) {
        if (candidate % primes[i] == 0)
            return 0
        i++
    }
    return 1
}
