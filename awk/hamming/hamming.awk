#!/usr/bin/env gawk -f

@include "assert"

{ 
    chars[NR][0] = ""
    n[NR] = split($0, chars[NR], //)
}

END {
    assert(n[1] == n[2], "Error: strands must be of equal length")

    for (i = 1; i <= n[1]; i++)
        if (chars[1][i] != chars[2][i])
            count++

    print 0 + count
}
