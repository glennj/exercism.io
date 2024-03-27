@include "join"

{
    limit = $1

    # create array of candidates
    delete candidates
    for (i = 1; i <= limit; i++)
        candidates[i] = 1
    candidates[1] = 0

    # mark non-primes as such
    mark_multiples(2, 2)
    for (i = 3; i <= sqrt(limit); i++)
        if (candidates[i])
            mark_multiples(i, 2*i)

    # get the list of primes
    n = 0
    delete primes
    for (i = 1; i <= limit; i++)
        if (candidates[i])
            primes[++n] = i

    print join(primes, 1, n, ",")
}

function mark_multiples(prime, step,    i) {
    for (i = prime * prime; i <= limit; i += step)
        candidates[i] = 0
}
