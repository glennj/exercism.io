#include "my_math.h"
#include <math.h>

unsigned digit_length(unsigned number) {
	if (number == 0) return 0;
    return floor(1 + log10(number));
}

unsigned uint_pow(unsigned base, unsigned exponent) {
    /* ref: https://stackoverflow.com/a/29787467/7552
    return (int)(pow(base, exponent) + 0.5);
    */

    // better:
    unsigned result = 1;
    while (exponent) {
        if (exponent % 2)
			result *= base;
		exponent /= 2;
        base *= base;
    }
    return result;
}
