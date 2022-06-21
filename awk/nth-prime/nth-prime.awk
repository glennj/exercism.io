#!/usr/bin/env gawk -f

BEGIN {
    # variable `n` comes from the command line

    if (n <= 0) {
        print "invalid input" > "/dev/stderr"
        exit 1
    }

    # initialize the primes array ("prime the pump", as it were)
    i=1; prime = 2; primes[i] = prime
    i++; prime = 3; primes[i] = prime
    while (i < n) {
        i++; prime = nextPrime(prime); primes[i] = prime
    }
    
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
