#include "sum_of_multiples.h"
#include <stdint.h>
#include <stdlib.h>

#if USE_BIT_ARRAY == 1

#include <stdio.h>
// https://web.cs.dal.ca/~jamie/UWO/BitVectors/README.html
#include "types.h"
#include "bitarr.h"


unsigned int sum(
        const unsigned int *factors,
        const size_t number_of_factors,
        const unsigned int limit
) {
	ba_init();
	bit *multiples = ba_new(limit);

    for (size_t i = 0; i < number_of_factors; i++)
        if (factors[i] > 0)
            for (unsigned mult = factors[i]; mult < limit; mult += factors[i])
                ba_assign(multiples, mult, TRUE);
	
    unsigned total = 0;
    for (size_t i = 0; i < limit; i++)
        if (ba_value(multiples, i))
            total += i;

    free(multiples);
    return total;
}

#else

unsigned int sum(
        const unsigned int *factors,
        const size_t number_of_factors,
        const unsigned int limit
) {
    uint8_t *multiples = calloc(limit, sizeof *multiples);

    for (size_t i = 0; i < number_of_factors; i++)
        if (factors[i] > 0)
            for (unsigned mult = factors[i]; mult < limit; mult += factors[i])
                multiples[mult] = 1;
    
    unsigned total = 0;
    for (size_t i = 0; i < limit; i++)
        if (multiples[i])
            total += i;

    free(multiples);
    return total;
}

/* inefficient O(n * m) solution:
 * for each number between 1 and limit, iterate over all the factors
 *
    #include <stdbool.h>

    static bool is_multiple(
            const unsigned int n,
            const unsigned int *factors,
            const size_t number_of_factors
    ) {
        for (size_t i = 0; i < number_of_factors; i++)
            if (factors[i] > 0 && n % factors[i] == 0)
                return true;
        return false;
    }

    unsigned int sum(
            const unsigned int *factors,
            const size_t number_of_factors,
            const unsigned int limit
    ) {
        unsigned total = 0;
        for (unsigned n = 1; n < limit; n++)
            if (is_multiple(n, factors, number_of_factors))
                total += n;
        return total;
    }
*/

#endif  // USE_BIT_ARRAY
