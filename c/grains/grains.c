#include "grains.h"
#include <stdint.h>

#define INDEX_MIN  1
#define INDEX_MAX 64

uint64_t square(uint8_t index) {
    return (index < INDEX_MIN || index > INDEX_MAX)
        ? 0
        : (uint64_t)1 << (index - 1);
}

uint64_t total(void) {
    return UINT64_MAX;
}
