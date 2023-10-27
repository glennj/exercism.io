#include "binary.h"


int convert(const char *p) {
    int result = 0;
    for (; *p; p++) {
        int digit = *p - '0';
        if (!(0 <= digit && digit < BASE))
            return INVALID;
        result = result * BASE + digit;
    }
    return result;
}
