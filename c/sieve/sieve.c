/* This solution is potentiall quite inefficient:
 * It will find the primes <= limit according to Eratosthenes (efficiently)
 * but it finds all of them, not just the first max_primes.
 * If limit is large and max_primes is small, I do too much work.
 */

#include "sieve.h"
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

static bool *
make_candidates_array(uint32_t limit) {
    bool *candidates = malloc((1 + limit) * sizeof *candidates);
    if (candidates != NULL)
        memset(candidates, true, 1 + limit);
    return candidates;
}

static void
mark_multiples_of(
        bool *candidates,
        unsigned n,
        unsigned step,
        uint32_t limit
) {
    for (unsigned i = n * n; i <= limit; i += step)
        candidates[i] = false;
}

static void
mark_multiples(
        bool *candidates,
        uint32_t limit
) {
    unsigned root = sqrt(limit);
    unsigned p = 2;
    mark_multiples_of(candidates, p, p, limit);
    for (p = 3; p <= root; p += 2)
        if (candidates[p])
            mark_multiples_of(candidates, p, 2 * p, limit);
}

static uint32_t
extract_primes(
        bool *candidates,
        uint32_t *primes,
        uint32_t limit,
        size_t max_primes
) {
    uint32_t j = 0;
    for (uint32_t i = 2; i <= limit; i++) {
        if (candidates[i]) {
            primes[j++] = i;
            if (j == max_primes)
                break;
        }
    }
    return j;
}

uint32_t sieve(uint32_t limit, uint32_t *primes, size_t max_primes) {
    bool *candidates = make_candidates_array(limit);
    if (candidates == NULL)
        return 0;

    mark_multiples(candidates, limit);
    uint32_t num_primes = extract_primes(candidates, primes, limit, max_primes);
    free(candidates);
    return num_primes;
}
