#!/usr/bin/env gawk -f

@include "assert"

# These variables are initialized on the command line (using '-v'):
# - len

BEGIN {
    assert(len >= 0, "slice length cannot be negative")
    assert(len > 0, "slice length cannot be zero")
}

{
    size = length
    assert(size, "series cannot be empty")
    assert(len <= size, "slice length cannot be greater than series length")

    i = 1
    printf "%s", substr($0, i, len)
    while (i + len <= size)
        printf " %s", substr($0, ++i, len)
    print ""
}
