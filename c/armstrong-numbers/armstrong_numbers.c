#include "armstrong_numbers.h"

bool is_armstrong_number(int candidate) {
    return candidate == armstrong_sum(candidate);
}

int armstrong_sum(int number) {
    int length = digit_length(number);
    int sum = 0;

    for (; number > 0; number /= 10) {
        sum += ipow(number % 10, length);
    }
    return sum;
}

int digit_length(int number) {
    return (int)ceil(log10(number));
}

int ipow(int base, int exponent) {
    // ref: https://stackoverflow.com/a/29787467/7552
    return (int)(pow(base, exponent) + 0.5);
}
