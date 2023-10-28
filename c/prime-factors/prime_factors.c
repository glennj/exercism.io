#include "prime_factors.h"
#include <stdint.h>
#include <stddef.h>
#include <math.h>

size_t find_factors(uint64_t n, uint64_t factors[static MAXFACTORS]) {
    uint64_t factor = 2, root = sqrt(n);
    size_t i = 0;

    while (factor <= root) {
        if (n % factor != 0) {
            factor += factor == 2 ? 1 : 2;
            continue;
        }
        if (i == MAXFACTORS)
            break;
        factors[i++] = factor;
        n /= factor;
    }

    if (n > 1 && i < MAXFACTORS)
        factors[i++] = n;

    return i;
}
