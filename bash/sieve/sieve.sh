#!/usr/bin/env bash
# shellcheck disable=SC2206

# Sieve of Eratosthenes

sieve() {
    local -i limit=$1 i
    local -a is_prime
    local -a primes
    local sieve_function=${2:-"sieve_optimized"}

    # create the array of candidates
    for ((i = 2; i <= limit; i++)); do
        is_prime[i]=true
    done

    # invoke the sieve function, and extract the prime numbers
    "$sieve_function"

    echo "${primes[*]}"
}

# unoptimized loop
#
# $ time bash sieve.sh 100000 "sieve_naive" >/dev/null
# 
# real  0m13.827s
# user  0m13.782s
# sys   0m0.023s

sieve_naive() {
    for ((p = 2; p <= limit; p++)); do
        # mark multiples as not prime
        for ((i = 2 * p; i <= limit; i += p)); do
            is_prime[i]=false
        done
    done

    for ((i = 2; i <= limit; i++)); do
        ${is_prime[i]} && primes+=($i)
    done
}

# a more optimized version
# these optimizations speed up the code a great deal:
# 
# $ time bash sieve.sh 100000 "sieve_optimized" >/dev/null
# 
# real  0m1.839s
# user  0m1.822s
# sys   0m0.009s

sieve_optimized() {
    # we can stop iterating when p <= sqrt(limit), all
    # the multiples will have been seen by then.
    for ((p = 2; p * p <= limit; p++)); do

        # if we know this p is non-prime, no need to remove
        # multiples of it: it's already been done
        ${is_prime[p]} || continue

        # while iterating when p > 2, we know we've already
        # handled any multiple of 2, and also that p will be
        # odd, so we can step by 2*p (e.g. 3, 9, 15, ...)
        ((step = p == 2 ? 2 : 2 * p))

        # we can start removing multiples starting at p * p
        # because for any multiple k*p where k < p, we have
        # already removed those numbers as non-prime in
        # previous iterations of p.
        for ((i = p * p; i <= limit; i += step)); do
            is_prime[i]=false
        done
    done

    for ((i = 2; i <= limit; i++)); do
        ${is_prime[i]} && primes+=($i)
    done
}

# mark non-primes by removing them from the array, then the
# remaining indices are the prime numbers. 
#
# This is quicker largely due to not having to loop over
# the numbers from 2 to limit to determine the primes, it
# just takes the array indices.
#
# $ time bash sieve.sh 100000 "sieve_arrayunset" >/dev/null
# 
# real  0m1.269s
# user  0m1.255s
# sys   0m0.007s

sieve_arrayunset() {
    for ((p = 2; p * p <= limit; p++)); do
        if [[ -n ${is_prime[p]} ]] && ${is_prime[p]}; then
            ((step = p == 2 ? 2 : 2 * p))
            for ((i = p * p; i <= limit; i += step)); do
                unset 'is_prime[i]'
            done
        fi
    done

    primes=(${!is_prime[@]})
}

sieve "$@"
