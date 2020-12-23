#!/usr/bin/env bash

# a library of useful bash functions
# works with bash version 3.2+

#############################################################
# Math functions

# Absolute value
#
math::abs() {
    echo $(( $1 < 0 ? -1 * $1 : $1 ))
}

# Greatest Common Divisor of two integers
#
math::gcd() {
    local a b
    a=$(math::abs "$1")
    b=$(math::abs "$2")
    if (( b > 0 )); then
        math::gcd "$b" $((a % b))
    else
        echo "$a"
    fi
}


# minimum/maximum of two values
#
math::min() { echo $(( $1 < $2 ? $1 : $2 )); }
math::max() { echo $(( $1 > $2 ? $1 : $2 )); }


# Add two arbitrarily large _positive_ integers, just like you'd do by hand:
# add digits from right to left, using a carry digit.
# 
math::add() {
    local a=$1 b=$2

    # left pad the numbers with zeroes so they're the same width
    local width
    width=$( math::max ${#a} ${#b} )
    printf -v a '%0*s' "$width" "$a"
    printf -v b '%0*s' "$width" "$b"

    # add the digits from right to left
    local c result="" carry=0
    for (( i = width-1; i >= 0; i-- )); do
        (( c = carry + ${a:i:1} + ${b:i:1} ))
        result=$((c % 10))${result}
        (( carry = c / 10 ))
    done
    result=${carry}${result}

    local extglob
    extglob=$(shopt -p extglob)
    shopt -s extglob

    # trim leading zeroes
    : "${result##+(0)}"
    echo "${_:-0}"

    $extglob
}


# a randomish number in the range [a,b)
# the $RANDOM variable returns a number in [0, 32768)
#
math::rand() {
    local a=$1 b=$2
    case $# in
        0) echo $RANDOM ;;
        1) echo $(( RANDOM % $1 )) ;;
        2) echo $(( $(math::min "$1" "$2") + RANDOM % ($2 - $1) )) ;;
        *) echo "usage: ${FUNCNAME[0]} [[lower] upper]" >&2; return 1;;
    esac
}


# sum the arguments
#
math::sum() {
    local sum=0
    for arg; do ((sum += arg)); done
    echo "$sum"
}

# sum the contents of an array
#
math::sumArray() {
    # namerefs are a v4.3 feature. indirect vars work in v3.2
    #local -n __sum_array=$1
    local ref="${1}[@]"
    local -a __sum_array=( "${!ref}" )
    math::sum "${__sum_array[@]}"
}


# floorMod: return the modulo in the range [0,n)
# Rationale: (-5 % 26) returns -5, but if we want
# to express it as a positive number it would be 21.
#
math::floorMod() {
    local num=$1 divisor=$2
    echo $(( ((num % divisor) + divisor) % divisor ))
}
