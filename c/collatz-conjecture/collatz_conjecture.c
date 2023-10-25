#include "collatz_conjecture.h"

static int do_steps(int start, int steps) {
    if (start == 1)     return steps;
    if (start % 2 == 0) return do_steps(start / 2, steps + 1);
    else                return do_steps(3 * start + 1, steps + 1);
}

int steps(int start) {
    if (start < 1) return ERROR_VALUE;
    return do_steps(start, 0);
}
