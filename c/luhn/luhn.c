#include "luhn.h"
#include <ctype.h>
#include <assert.h>

static int doubled[2][10] = {
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
    {0, 2, 4, 6, 8, 1, 3, 5, 7, 9},
};

bool luhn(const char *num) {
    assert(num);

    // navigate to the last char of the string
    const char *p = num;
    while (*p) p++;
    p--;

    bool dbl = 0;
    int sum = 0, ndigits = 0;

    for (; p >= num; p--) {
        if (isspace(*p)) continue;
        if (!isdigit(*p)) return false;

        sum += doubled[dbl][*p - '0'];
        dbl = !dbl;
        ndigits++;
    }

    return sum == 0
            ? ndigits > 1    // multiple zeros is valid
            : sum % 10 == 0;
}
