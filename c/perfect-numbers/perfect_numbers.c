#include "perfect_numbers.h"
#include <math.h>

static int aliquot_sum(int number) {
    int sum = 0;
    int root = sqrt(number);
    for (int f = 1; f <= root; f++) {
        if (number % f == 0) {
            sum += f;
            int g = number / f;
            if (f < g)
                sum += g;
        }
    }
    return sum - number;
}

kind classify_number(int number) {
    if (number < 1)
        return ERROR;

    int sum = aliquot_sum(number);

    if (sum < number)
        return DEFICIENT_NUMBER;
    else if (sum > number)
        return ABUNDANT_NUMBER;
    else
        return PERFECT_NUMBER;
}
