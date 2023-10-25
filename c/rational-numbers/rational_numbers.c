#include "rational_numbers.h"
#include "my_math.h"
#include <stdlib.h>
#include <math.h>

static rational_t rationalize(int numerator, int denominator) {
    if (denominator < 0) {
        numerator *= -1;
        denominator *= -1;
    }
    int g = gcd(abs(numerator), denominator);
    return (rational_t){ numerator / g, denominator / g };
}

rational_t reduce(rational_t r) {
    return rationalize(r.numerator, r.denominator);
}

rational_t add(rational_t r1, rational_t r2) {
    int num = r1.numerator * r2.denominator + r2.numerator * r1.denominator;
    int denom = r1.denominator * r2.denominator;
    return rationalize(num, denom);
}

rational_t negate(rational_t r) {
    return rationalize(-r.numerator, r.denominator);
}

rational_t subtract(rational_t r1, rational_t r2) {
    return add(r1, negate(r2));
}

rational_t multiply(rational_t r1, rational_t r2) {
    int num = r1.numerator * r2.numerator;
    int denom = r1.denominator * r2.denominator;
    return rationalize(num, denom);
}

rational_t invert(rational_t r) {
    return rationalize(r.denominator, r.numerator);
}

rational_t divide(rational_t r1, rational_t r2) {
    return multiply(r1, invert(r2));
}

rational_t absolute(rational_t r) {
    return rationalize(abs(r.numerator), abs(r.denominator));
}

rational_t exp_rational(rational_t r, int16_t n) {
    if (n < 0)
        return exp_rational(invert(r), -n);

    int num = int_pow(r.numerator, n);
    int denom = int_pow(r.denominator, n);
    return rationalize(num, denom);
}

float exp_real(uint16_t x, rational_t r) {
    return nthroot(powf(x, r.numerator),
                   abs(r.denominator));
}
