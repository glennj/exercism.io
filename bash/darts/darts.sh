#!/usr/bin/env bash

source ./utils.bash
source ./utils_string.bash

############################################################
# This uses a single call to bc: bc can return several results.
score_throw_bc() {
    local x=$1 y=$2
    local one five ten
    # shellcheck disable=SC2162
    { read one; read five; read ten; } < <(
        bc -l << _END_
            x = sqrt($x^2 + $y^2)
            x <= 1
            x <= 5
            x <= 10
_END_
    )
    ((one))  && return 10
    ((five)) && return 5
    ((ten))  && return 1 || return 0
}

############################################################
# Given an x and y, we will calculate sqrt(x^2 + y^2) and
# compare that against some radii to determine the score.
#
# However bash has not implemented floating point arithmetic.
#
# We could call out to an external tool like awk:
#    awk -v "x=$1" -v "y=$2" 'BEGIN {
#        hypot = sqrt(x^2 + y^2)
#        if      (hypot <=  1) print 10
#        else if (hypot <=  5) print  5
#        else if (hypot <= 10) print  1
#        else                  print  0
#    }'
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
#
score_throw_bash_only() {
    checkBashVersion 4.3 namerefs
    source ./utils_math.bash

    local x=$1 y=$2
    local -i exp=0

    if [[ $x == *.* || $y == *.* ]]; then
        # suppose x=0.04 and y=12
        # then `asintegers x y exp` will set the following values:
        #    exp=2, x=4, y=1200
        asintegers x y exp
    fi
    local -i hypot2=$((x * x + y * y))

    local -a scores=([1]=10 [5]=5 [10]=1)
    local score=0
    for dist in "${!scores[@]}"; do
        if ((hypot2 <= (dist * 10 ** exp) ** 2)); then
            score="${scores[dist]}"
            break
        fi
    done
    echo "$score"
}

# ref: https://en.wikipedia.org/wiki/Floating-point_arithmetic
# Given a floating point number 1.2345, we want to determine
# the significand (12345) and the exponent (-4), but express
# the exponent as a positive int.
#
asintegers() {
    local -n f=$1 g=$2 e=$3
    local -i fexp gexp

    [[ $f == *.* ]] && fexp=$(exponent "$f") || fexp=0
    [[ $g == *.* ]] && gexp=$(exponent "$g") || gexp=0
    e=$(math::max $fexp $gexp)

    if ((e > 0)); then
        # We will eventually square the number, so
        # it's OK to strip off the leading hyphen.
        # Use printf to right-pad each number with zeroes
        # so that they're the same length
        printf -v f "%.*f" "$e" "${f#-}"
        printf -v g "%.*f" "$e" "${g#-}"

        # Now, remove the decimal point.
        # Force interpretation as base-10 numbers so that
        # if f is say 0.8 then we don't get
        # "invalid octal" errors.
        f=$((10#${f/./}))
        g=$((10#${g/./}))
    fi
}

exponent() {
    local float=$1
    local frac=${float#*.}
    echo ${#frac}
}

############################################################
main() {
    assert    $# == 2           "wrong number of arguments"
    assert -C str::isFloat "$1" "arguments must be numeric"
    assert -C str::isFloat "$2" "arguments must be numeric"

    #score_throw_bash_only "$@"
    score_throw_bc "$@"; echo $?
}

main "$@"
