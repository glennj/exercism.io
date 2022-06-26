#!/usr/bin/env gawk -f

@include "assert"

# These variables are initialized on the command line (using '-v'):
# - len

BEGIN {
    assert(len > 0, "invalid length")
}
{
    size = length
    assert(size, "series cannot be empty")
    assert(len <= size, "invalid length")

    i = 1
    printf "%s", substr($0, i, len)
    while (i + len <= size)
        printf " %s", substr($0, ++i, len)
    print ""
}
