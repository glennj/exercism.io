#include "reverse_string.h"
#include <stdlib.h>

// for fun, implement `strlen`
static size_t strlen(const char *str) {
    size_t len = 0;
    for (; *str; str++, len++);
    return len;
}

char *reverse(const char *value) {
    if (!value)
        return NULL;

    size_t len = strlen(value);

    char *reversed = calloc(len + 1, sizeof *reversed);

    const char *p = value;
    char *q = reversed + len - 1;

    for (; *p; p++, q--)
        *q = *p;

    return reversed;
}
