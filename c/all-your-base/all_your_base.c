#include "all_your_base.h"

static void reverse(int8_t array[], size_t length) {
    int8_t *p = array;
    int8_t *q = array + length - 1;
    int8_t tmp;
    for (; p < q; p++, q--) {
        tmp = *p;
        *p = *q;
        *q = tmp;
    }
}

static int valid (
        int8_t digits[],
        int16_t input_base,
        int16_t output_base,
        size_t input_length
) {
    if (input_length == 0 || input_base < 2 || output_base < 2)
        return 0;

    // yeah, yeah, I know this is an extra loop over the digits
    for (size_t i = 0; i < input_length; i++)
        if (digits[i] < 0 || digits[i] >= input_base)
            return 0;

    return 1;
}

// ------------------------------------------------------------
size_t rebase (
        int8_t digits[],    // in-out parameter
        int16_t input_base,
        int16_t output_base,
        size_t input_length
) {
    // garbage in, garbage out
    if (input_length > DIGITS_ARRAY_SIZE)
        input_length = DIGITS_ARRAY_SIZE;

    if (!valid(digits, input_base, output_base, input_length))
        return 0;

    // to decimal
    unsigned int decimal = 0;
    for (size_t i = 0; i < input_length; i++) {
        decimal = decimal * input_base + digits[i];
        digits[i] = 0;  // clean up behind us
    }

    if (decimal == 0)
        return 1;       // result is 1 zero digit

    size_t count = 0;
    // this loop writes the output digits in reverse order
    while (decimal > 0 && count < DIGITS_ARRAY_SIZE) {
        digits[count++] = decimal % output_base;
        decimal /= output_base;
    }
    reverse(digits, count);

    return count;
}
