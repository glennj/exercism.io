#!/usr/bin/env bash

# Sieve of Eratosthenes

sieve() {
    local -i limit=$1 i
    local -a is_prime
    local -ia primes

    # create the array of candidates
    for ((i = 2; i <= limit; i++)); do
        is_prime[i]=true
    done

    # un-mark the non-primes in the is_prime array

    #sieve_naive
    sieve_optimized
    #sieve3

    # extract the prime numbers
    for ((i = 2; i <= limit; i++)); do
        ${is_prime[i]} && primes+=($i)
    done

    echo "${primes[*]}"

}

# unoptimized loop
sieve_naive() {
    for ((p = 2; p <= limit; p++)); do
        # mark multiples as not prime
        for ((i = 2 * p; i <= limit; i += p)); do
            is_prime[i]=false
        done
    done
}

# a more optimized version
# these optimizations speed up the code a great deal:
#
# $ time bash sieve_unoptimized.sh 100000 >/dev/null
#
# real	0m17.402s
# user	0m17.375s
# sys	0m0.018s
#
# $ time bash sieve.sh 100000 >/dev/null
#
# real	0m2.377s
# user	0m2.366s
# sys	0m0.007s

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
}

# I thought this might have been faster: mark non-primes by
# removing them from the array, then the remaining indices
# are the prime numbers. It is however 2x slower than the
# "optimized" version -- I presume due to the overhead of
# removing an element from the linked list implementation of
# bash indexed arrays.
sieve3() {
    for ((p = 2; p * p <= limit; p++)); do
        if [[ -n ${is_prime[p]} ]] && ${is_prime[p]}; then
            ((step = p == 2 ? 2 : 2 * p))
            for ((i = p * p; i <= limit; i += step)); do
                unset 'is_prime[i]'
            done
        fi
    done
    # output the indices and exit
    echo "${!is_prime[*]}"
    exit
}

sieve "$@"
