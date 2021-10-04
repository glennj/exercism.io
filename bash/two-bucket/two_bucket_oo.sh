#!/usr/bin/env bash

# This is an "object oriented" style.
# More notes in the Bucket class file.

# shellcheck disable=SC2086,SC2155,SC2046

source ./utils.bash
source ./utils_math.bash
source ./bucket_class.bash

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
        satisfied $goal $first $second $moves && return
        satisfied $goal $second $first $moves && return

        if   $first isEmpty; then $first fill
        elif $second isFull; then $second empty
        else                      $first pourInto $second
        fi

        ((moves += 1))
    done
}

satisfied() {
    local goal=$1 a=$2 b=$3 moves=$4
    (($($a get amount) == goal)) || return 1
    echo "moves: $moves," \
         "goalBucket: $($a get name)," \
         "otherBucket: $($b get amount)"
}

validate() {
    local -i size1=$1 size2=$2 goal=$3

    local max=$(math::max $size1 $size2)
    assert "goal <= max" "invalid goal: too big"

    local gcd=$(math::gcd $size1 $size2)
    assert "gcd == 1 || (goal % gcd) == 0" "invalid goal: unsatisfiable"
}

main() {
    validate "${@:1:3}"

    Bucket new x name "one" size "$1"
    Bucket new y name "two" size "$2"

    case $4 in
        one) solve x y $3 ;;
        two) solve y x $3 ;;
        *)   die "invalid start bucket: $start" ;;
    esac
}

main "$@"
