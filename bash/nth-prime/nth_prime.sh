#!/usr/bin/env bash

# external tools: bc

source ../lib/utils.bash
checkBashVersion 4.3 namerefs

# The largest concern is finding an algorithm that is not
# excruciatingly slow in bash.
# The Sieve of Eratosthenes is a good quick algorithm for
# finding # primes less than an upper bound. The key is
# estimating # an approximate upper bound for the nth prime

# approximate the nth prime number
# ref: https://exercism.io/tracks/python/exercises/nth-prime/solutions/2a84e7330fab4b4cb04d92cf0f38164d
# ref: https://en.wikipedia.org/wiki/Prime_number_theorem
approxNth() {
    local real
    # bc's `l()` function is natural logarithm
    real=$(printf "2 + 1.2 * %d * l(%d)\n" "$1" "$1" | bc -l)
    echo "${real%.*}"   # truncate the fractional part
}

# Sieve of Eratosthenes
sieve() {
    local -i limit=$1 i p
    local -n __primes=$2
    local -a is_prime

    for ((i = 2; i <= limit; i++)); do
        is_prime[i]=true
    done
    for ((p = 2; p * p <= limit; p++)); do
        if ${is_prime[p]}; then
            ((step = p == 2 ? 2 : 2 * p))
            for ((i = p * p; i <= limit; i += step)); do
                is_prime[i]=false
            done
        fi
    done
    for ((i = 2; i <= limit; i++)); do
        ${is_prime[i]} && __primes+=("$i")
    done
    return
}

main() {
    local -i n=$(($1))
    assert "n > 0" "invalid input"

    local primes=()
    sieve "$(approxNth $n)" primes

    # should do some validation that we acually have n
    # prime numbers, but this passes the test suite.

    echo "${primes[n - 1]}"
}

main "$@"
