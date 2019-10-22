#!/usr/bin/env bash

# namerefs introduced in bash 4.3
if    [[ ${BASH_VERSINFO[0]} -lt 4 ]] ||
    { [[ ${BASH_VERSINFO[0]} -eq 4 ]] && [[ ${BASH_VERSINFO[1]} -lt 3 ]]; }
then
    echo "bash version 4.3 required" >&2
    exit 2
fi

# Given an x and y, we will calculate sqrt(x^2 + y^2) and
# compare that against some radii to determine the score.
#
# However bash has not implemented floating point arithmetic.
#
# We could call out to an external tool like awk.
#
# Or we do some manipulation to convert the numbers to integers:
# Suppose x=0.03 and y=3.0
# Our comparision will be: sqrt(0.03 ^ 2 + 3.0 ^2) <= 5 
# ==                      0.03 ^ 2 + 3 ^ 2 <= 5 ^ 2 
# ==             10 ^ 4 (0.03 ^ 2 + 3 ^ 2) <= 10 ^ 4 (5 ^ 2) 
# == (0.03 * 10 ^2) ^ 2 + (3 * 10 ^ 2) ^ 2 <= (5 * 10 ^ 2) ^ 2 
# ==                       3 ^ 2 + 300 ^ 2 <= 500 ^ 2 
#
# The goal is to find the exponent of 10 that converts x and y
# into integers. Now we have arithmetic that bash can do.

shopt -s extglob

main() {
    (( $# == 2 )) || die "wrong number of arguments"
    isnumeric "$1" && isnumeric "$2" || die "arguments must be numeric"
    score_throw "$@"
}

die() { echo "#*" >&2; exit 1; }

isnumeric() {
    [[ $1 == ?([+-])+([[:digit:]])?(.+([[:digit:]])) ]]
}

score_throw() {
    local x=$1 y=$2 
    local -i exp=0

    if [[ $x == *.* || $y == *.* ]]; then
        # suppose x=0.04 and y=12
        # then `asintegers x y exp` will set the following values:
        #    exp=2, x=4, y=1200
        asintegers x y exp
    fi
    local -i hypot2=$(( x*x + y*y ))

    local -a scores=( [1]=10 [5]=5 [10]=1 )
    local score=0
    for dist in "${!scores[@]}"; do
        if (( hypot2 <= ( dist * 10 ** exp) ** 2 )); then 
            score="${scores[dist]}"
            break
        fi
    done
    echo $score
}

# ref: https://en.wikipedia.org/wiki/Floating-point_arithmetic
# Given a floating point number 1.2345, we want to determine
# the significand (12345) and the exponent (-4), but express 
# the exponent as a positive int.
#
asintegers() {
    local -n f=$1 g=$2 e=$3
    local -i fexp gexp

    [[ $f == *.* ]] && fexp=$(exponent $f) || fexp=0
    [[ $g == *.* ]] && gexp=$(exponent $g) || gexp=0
    e=$( max $fexp $gexp )

    if (( e > 0 )); then
        # We will eventually square the number, so 
        # it's OK to strip off the leading hyphen.
        # Use printf to right-pad each number with zeroes
        # so that they're the same length
        printf -v f "%.*f" $exp ${f#-}
        printf -v g "%.*f" $exp ${g#-}

        # Now, remove the decimal point.
        # Force interpretation as base-10 numbers so that
        # if f is say 0.8 then we don't get 
        # "invalid octal" errors.
        f=$(( 10#${f/./} ))
        g=$(( 10#${g/./} ))
    fi
}

exponent() {
    local float=$1
    local frac=${float#*.}
    echo ${#frac}
}

max() { echo $(( $1 > $2 ? $1 : $2 )); }

main "$@"
