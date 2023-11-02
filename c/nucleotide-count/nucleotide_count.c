#include "nucleotide_count.h"
#include <stdlib.h>
#include <stdio.h>
#include "my_math.h"

static unsigned digit_width(unsigned n) {
    return n == 0 ? 1 : digit_length(n);
}

char *count(const char *strand) {
    unsigned a = 0, c = 0, g = 0, t = 0; 
    char *result = calloc(1, sizeof *result);

    for(; *strand; strand++) {
        switch (*strand) {
            case 'A': a++; break;
            case 'C': c++; break;
            case 'G': g++; break;
            case 'T': t++; break;
            default: return result;
        }
    }

    int width = 2 + digit_width(a) + 1
              + 2 + digit_width(c) + 1
              + 2 + digit_width(g) + 1
              + 2 + digit_width(t) + 1;
    result = realloc(result, width * sizeof *result);

    sprintf(result, "A:%d C:%d G:%d T:%d", a, c, g, t);
    return result;
}
