#!/usr/bin/env bash

# Since we're dealing with numbers here, we can be relaxed about quoting.
# shellcheck disable=SC2086

# A library of math-related bash functions.
# Works with bash version 3.2+

if ! type -t with_shopt > /dev/null; then
    # shellcheck source=/dev/null
    source "$(dirname "${BASH_SOURCE[0]}")"/utils.bash
fi

#############################################################
# Math functions

math::abs() { echo $(($1 < 0 ? -1 * $1 : $1)); }

# max and min take varargs params
math::max() {
    local -i max=$1 n
    shift
    for n; do ((max = max > n ? max : n)); done
    echo $max
}

math::min() {
    local -i min=$1 n
    shift
    for n; do ((min = min < n ? min : n)); done
    echo $min
}

# floorMod: return the modulo in the range [0,n)
# Rationale: (-5 % 26) returns -5, but if we want
# to express it as a positive number it would be 21.
#
math::floorMod() {
    local -i num=$1 divisor=$2
    echo $(( ((num % divisor) + divisor) % divisor ))
}

# Greatest Common Divisor of two integers
#
math::gcd() {
    local -i a b
    a=$(math::abs "$1")
    b=$(math::abs "$2")
    if ((b > 0)); then
        "${FUNCNAME[0]}" $b $((a % b))
    else
        echo $a
    fi
}

# Modular multiplicative inverse
# https://en.wikipedia.org/wiki/Modular_multiplicative_inverse
#
math::mmi() {
    if (($# != 2)); then
        echo "usage: ${FUNCNAME[0]} a m" >&2
        return 1
    fi
    local -i gcd
    gcd=$(math::gcd $1 $2)
    if ((gcd != 1)); then
        echo "$1 and $2 must be coprime" >&2
        return 1
    fi
    local -i mmi i
    for ((i = 1, mmi = 0; i <= $2; i++)); do
        ((mmi = (mmi + $1) % $2))
        if ((mmi == 1)); then
            echo $i
            return 0
        fi
    done
    echo "Error: cannot find MMI of $1 mod $2" >&2
    return 1
}

# Add two arbitrarily large integers, just like you'd do by
# hand: add digits from right to left, using a carry digit.
#
math::add() {
    local a=$1 b=$2 c
    local result="" carry=0

    # left pad the numbers with zeroes so they're the same width
    local width
    width=$(math::max ${#a} ${#b})
    printf -v a '%0*s' $width $a
    printf -v b '%0*s' $width $b

    # add the digits from right to left
    for ((i = width - 1; i >= 0; i--)); do
        ((c = carry + ${a:i:1} + ${b:i:1}))
        result=$((c % 10))${result}
        ((carry = c / 10))
    done
    result=${carry}${result}

    # shellcheck disable=SC2016
    with_shopt extglob 'echo "${result##+(0)}"'
}

# a randomish number in the range [a,b)
#
math::rand() {
    local -i a=$1 b=$2
    local -i r=$((RANDOM % (b - a)))
    echo $((r + a))
}

# sum the arguments
#
math::sum() {
    local sum=0
    for arg; do ((sum += arg)); done
    echo $sum
}

# sum the contents of an array
#
math::sumArray() {
    local -n __sum_array=$1
    math::sum "${__sum_array[@]}"
}
