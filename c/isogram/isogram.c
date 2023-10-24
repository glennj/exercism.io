#include "isogram.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <ctype.h>

bool is_isogram(const char phrase[]) {
    if (phrase == NULL)
        return false;

    int32_t bitfield = 0;
    for (; *phrase; phrase++) {
        char c = *phrase;
        if (isalpha(c)) {
            int32_t mask = 1 << (tolower(c) - 'a');
            if (bitfield & mask)
                return false;
            bitfield |= mask;
        }
    }
    return true;
}

/* alternate approach: array of bools
 * https://exercism.org/tracks/c/exercises/isogram/solutions/siebenschlaefer
 */
