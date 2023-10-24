#include "armstrong_numbers.h"
#include "my_math.h"

bool is_armstrong_number(int candidate) {
    unsigned n = candidate;
    unsigned len = digit_length(n);
    int sum = 0;

    for (; n > 0; n /= 10) {
        sum += uint_pow(n % 10, len);
    }

    return candidate == sum;
}
