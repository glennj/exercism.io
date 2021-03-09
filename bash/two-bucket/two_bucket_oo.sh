#!/usr/bin/env bash

# This is an "object oriented" style.
# More notes in the Bucket class file.

# shellcheck disable=SC2086,SC2155,SC2046

source ../lib/utils.bash
source ../lib/utils_math.bash
source ./bucket_class.bash

validate() {
    local -i size1=$1 size2=$2 goal=$3

    # the goal amount must fit in a single bucket
    local max=$(math::max $size1 $size2)
    assert "goal <= max" "invalid goal: too big"

    # if the buckets are not relatively prime, then
    # the goal must be divisible by the greatest
    # common divisor of the buckdets
    local gcd=$(math::gcd $size1 $size2)
    assert "gcd == 1 || (goal % gcd) == 0" "invalid goal: unsatisfiable"
}

solve() {
    local first=$1 second=$2
    local -i goal=$3 moves=0

    $first fill
    ((moves += 1))

    if (($($second get size) == goal)); then
        $second fill
        ((moves += 1))
    fi

    while true; do
        if (($($first get amount) == goal)); then
            result $moves $($first get name) $($second get amount)
            return
        fi
        if (($($second get amount) == goal)); then
            result $moves $($second get name) $($first get amount)
            return
        fi

        if   $first isEmpty; then $first fill
        elif $second isFull; then $second empty
        else                      $first pourInto $second
        fi

        ((moves += 1))
    done
}

result() {
    printf "moves: %d, goalBucket: %s, otherBucket: %d\n" "$@"
}

main() {
    validate "${@:1:3}"

    Bucket new x name "one" size "$1"
    Bucket new y name "two" size "$2"

    local -i goal=$3
    local start=$4

    case $start in
        one) solve x y $goal ;;
        two) solve y x $goal ;;
        *)   die "invalid start bucket: $start" ;;
    esac
}

main "$@"
