#include "my_math.h"
#include <math.h>

unsigned
digit_length(unsigned number) {
	if (number == 0) return 0;
    return floor(1 + log10(number));
}

int
int_pow(int base, unsigned exponent) {
    /* ref: https://stackoverflow.com/a/29787467/7552
    return (int)(pow(base, exponent) + 0.5);
    */

    int result = 1;
    while (exponent) {
        if (exponent % 2)
			result *= base;
		exponent /= 2;
        base *= base;
    }
    return result;
}

unsigned
gcd(unsigned a, unsigned b) {
    if (b == 0) return a;
    return gcd(b, a % b);
}

float
nthroot(float num, unsigned root) {
    return powf(expf(1), logf(num) / root);
}
