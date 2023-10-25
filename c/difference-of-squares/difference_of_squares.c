#include "difference_of_squares.h"
#include <stdlib.h>

unsigned int sum_of_squares(unsigned int number) {
    unsigned result = 0;
    for (unsigned n = 1; n <= number; n++)
        result += n * n;
    return result;
}

unsigned int square_of_sum(unsigned int number) {
    unsigned sum = 0;
    for (unsigned n = 1; n <= number; n++) 
        sum += n;
    return sum * sum;
}

unsigned int difference_of_squares(unsigned int number) {
    int a = sum_of_squares(number);
    int b = square_of_sum(number);
    return abs(a - b);
}
