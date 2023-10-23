#ifndef ARMSTRONG_NUMBERS_H
#define ARMSTRONG_NUMBERS_H

#include <stdbool.h>
#include <math.h>

bool is_armstrong_number(int candidate);

int armstrong_sum(int number);
int digit_length(int number);
int ipow(int base, int exponent);

#endif

