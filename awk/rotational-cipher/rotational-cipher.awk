#!/usr/bin/env gawk -f

# These variables are initialized on the command line (using '-v'):
# - distance

BEGIN {
    char_map("abcdefghijklmnopqrstuvwxyz", lower)
    char_map("ABCDEFGHIJKLMNOPQRSTUVWXYZ", upper)
    FS = ""
}

function char_map(str, ary,    n, i, chars) {
    n = split(str, chars, "")
    for (i = 1; i <= n; i++)
        ary[chars[i]] = i - 1
}

{
    for (i = 1; i <= NF; i++) {
        char = $i
        if ($i in lower) char = rotate(char, lower, distance)
        if ($i in upper) char = rotate(char, upper, distance)
        printf "%s", char
    }
    print ""
}

function rotate(c, cs, n) {
    idx = (cs[c] + n) % length(cs)
    for (c in cs)
        if (cs[c] == idx)
            return c
}
