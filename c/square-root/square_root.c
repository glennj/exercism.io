#include "square_root.h"
#include <stdint.h>
#include <math.h>

/* Using the Binary numeral system implementation from
 * https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_(base_2)
 */

uint16_t square_root(uint32_t n) {
    // find b, the greatest power of 4 less than or equal to n
    uint32_t b = pow(4, floor(log(n) / log(4)));
    uint16_t x = 0;

    while (b != 0) {
        if (n >= x + b) {
            n = n - x - b;
            x = (x >> 1) + b;
        }
        else {
            x >>= 1;
        }
        b >>= 2;
    }

    return x;
}
