#!/bin/bash
# Using the Binary numeral system implementation from
# https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_(base_2)

sqrt() {
    local -i n=$1

    # find b, the greatest power of 4 less than or equal to n
    local -i b=1
    while ((b * 4 <= n)); do ((b *= 4)); done

    local -i x=0
    while ((b != 0)); do
        if ((n >= x + b)); then
            ((n = n - x - b))
            ((x = x/2 + b))
        else
            ((x /= 2))
        fi
        ((b /= 4))
    done
    echo $x
}

sqrt "$1"
