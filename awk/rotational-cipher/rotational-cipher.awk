#!/usr/bin/env gawk -f

# These variables are initialized on the command line (using '-v'):
# - distance

BEGIN {
    FS = ""
    char_map("abcdefghijklmnopqrstuvwxyz", lower)
    char_map("ABCDEFGHIJKLMNOPQRSTUVWXYZ", upper)
}

function char_map(str, array,    n, i, chars) {
    n = split(str, chars)
    for (i = 1; i <= n; i++)
        # value chosen to correlate to the `%` operator
        array[chars[i]] = i - 1
}

{
    for (i = 1; i <= NF; i++)  {
        char = $i
        if ($i in lower) char = rotate($i, lower, distance)
        if ($i in upper) char = rotate($i, upper, distance)
        printf "%s", char
    }
    print ""
}

function rotate(char, characters, n) {
    idx = (characters[char] + n) % length(characters)
    for (c in characters)
        if (characters[c] == idx)
            return c
}
