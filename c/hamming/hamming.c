#include "hamming.h"

int compute(const char *lhs, const char *rhs) {
    // if either pointer is null, bail out immediately
    if (!lhs || !rhs) return -1;

    int distance = 0;
    for (unsigned i = 0; ; i++) {
        char left = *(lhs + i);
        char right = *(rhs + i);

        if (!left && !right) break;     // end of both strings
        if (!left || !right) return -1; // end of one string
        if (left != right) distance++;
    }

    return distance;
}
