#include "pangram.h"
#include <stddef.h>
#include <stdint.h>
#include <ctype.h>

#define IS_PANGRAM 0x3ffffff    // 26 bits

bool is_pangram(const char *sentence) {
    uint32_t bitfield = 0;

    if (sentence)
        for (; *sentence && bitfield < IS_PANGRAM; sentence++)
            if (isalpha(*sentence))
                bitfield |= 1 << (tolower(*sentence) - 'a');

    return bitfield == IS_PANGRAM;
}
