#include "armstrong_numbers.h"

bool is_armstrong_number(int candidate) {
    int sum = 0;
    int n = candidate;

    // ref https://exercism.io/tracks/c/exercises/armstrong-numbers/solutions/44339a727c9041d4aec58435a4df24bc
    // int len = ceil(log10(candidate));
    int len = floor(log10(candidate) + 1);

    while (n > 0) {
        sum += pow(n % 10, len);
        n /= 10;
    }

    return (sum == candidate);
}

