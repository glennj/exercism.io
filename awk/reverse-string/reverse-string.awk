#!/usr/bin/env gawk -f
@include "join"

#########################################
# a complex method, less efficient
0 {
    n = split($0, chars, "")
    i = 1
    j = n
    while (i < j) 
        swap(chars, i++, j--)
    print join(chars, 1, n, SUBSEP)
}

function swap(a, i, j,    tmp) {
    tmp = a[i]
    a[i] = a[j]
    a[j] = tmp
}

#########################################
# simpler
BEGIN {FS = ""}
{ for (i = NF; i >= 1; i--) printf "%s", $i; print "" }
