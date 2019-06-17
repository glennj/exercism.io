#!/usr/bin/env bash

# Sieve of Eratosthenes

declare -i limit=$1
declare -i i p step

is_prime=()
for ((i = 2; i <= limit; i++)); do
    is_prime[i]=true
done

# unoptimized loop:
#
# for ((p = 2; p <= limit; p++)); do
#     # mark multiples as not prime
#     for (( i = 2 * p; i <= limit; i += p )); do
#         is_prime[i]=false
#     done
# done

# we can stop iterating when p <= sqrt(limit), all
# the multiples will have been seen by then.

for ((p = 2; p * p <= limit; p++)); do

    # if we know this p is non-prime, no need to remove
    # multiples of it: it's already been done

    if ${is_prime[p]}; then

        # while iterating when p > 2, we know we've already
        # handled any multiple of 2, and also that p will be
        # odd, so we can step by 2*p (e.g. 3, 9, 15, ...)
        #
        # note: we can assign to variables inside an
        # arithmetic expression

        (( step = (p==2) ? 2 : 2*p ))

        # we can start removing multiples starting at p * p
        # because for any multiple k*p where k < p, we have
        # already removed those numbers as non-prime in
        # previous iterations of p.

        for (( i = p * p; i <= limit; i += step )); do
            is_prime[i]=false
        done

    fi
done

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

primes=()
for ((p = 2; p <= limit; p++)); do
    ${is_prime[p]} && primes+=( $p )
done

echo "${primes[*]}"
