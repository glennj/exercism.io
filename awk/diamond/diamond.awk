#!/usr/bin/env gawk -f

@load "ordchr"
@include "join"

BEGIN { A = ord("A") }
{
    size = ord($1) - A + 1
    height = 2 * size - 1

    for (i = 1; i <= size; i++) {
        for (j = 1; j <= size; j++) {
            char = i == j ? chr(A + j - 1) : " "
            # fun with index arithmetic
            output[i][size + j - 1] = char
            output[i][size - j + 1] = char
            output[height - i + 1][size + j - 1] = char
            output[height - i + 1][size - j + 1] = char
        }
    }

    for (i in output)
        print join(output[i], 1, height, SUBSEP)
}

