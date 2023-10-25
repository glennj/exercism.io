#include "complex_numbers.h"
#include <math.h>

complex_t c_add(complex_t a, complex_t b) {
    return (complex_t){
        a.real + b.real,
        a.imag + b.imag
    };
}

complex_t c_sub(complex_t a, complex_t b) {
    return (complex_t){
        a.real - b.real,
        a.imag - b.imag
    };
}

complex_t c_mul(complex_t a, complex_t b) {
    return (complex_t){
        a.real * b.real - a.imag * b.imag,
        a.imag * b.real + a.real * b.imag
    };
}

complex_t c_div(complex_t a, complex_t b) {
    double denom = b.real * b.real + b.imag * b.imag;
    return (complex_t){
        (a.real * b.real + a.imag * b.imag) / denom,
        (a.imag * b.real - a.real * b.imag) / denom
    };
}

double c_abs(complex_t x) {
    return hypot(x.real, x.imag);
}

complex_t c_conjugate(complex_t x) {
    return (complex_t){x.real, -x.imag};
}

double c_real(complex_t x) {
    return x.real;
}

double c_imag(complex_t x) {
    return x.imag;
}

// Raising e to a complex exponent can be expressed as
//      `e^(a + i * b) = e^a * e^(i * b)`
// the last term of which is given by Euler's formula
//      `e^(i * b) = cos(b) + i * sin(b)`
complex_t c_exp(complex_t x) {
    complex_t a = {exp(x.real), 0};
    complex_t b = {cos(x.imag), sin(x.imag)};
    return c_mul(a, b);
}
