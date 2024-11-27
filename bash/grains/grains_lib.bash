#!/usr/bin/env bash
# shellcheck disable=SC2086

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    echo "This library of functions should be sourced into another script" >&2
    exit 4
fi

source ./utils_math.bash   # `math::add`

# using function "namespaces" to differentiate implemenations

# for benchmarking the different implemenations, please see
# https://github.com/glennj/exercism.io/tree/main/bash/grains

############################################################
# implementation calling out to `bc`

bc::atSquare() { bc <<< "2 ^ ($1 - 1)"; }

bc::total() {
    # this implementation, while nice and DRY,
    # is calling bc 65 times:
    #for i in {1..64}; do
    #    pow2 $i
    #done | paste -sd + | bc

    expression="0"
    for i in {1..64}; do
        expression+=" + (2^($i-1))"
    done
    bc <<< "$expression"
}

############################################################
# A version with no external tools used.
#
# I do not recommend this approach if you care about performance.

bash_only::calcSquares() {
    local OPTIND calcTotal=false
    while getopts "T" opt; do
        [[ $opt == T ]] && calcTotal=true
    done
    shift $((OPTIND - 1))

    square=(0 1)
    $calcTotal && total=${square[1]}

    for ((i = 2; i <= $1; i++)); do
        square[i]=$(math::add ${square[i - 1]} ${square[i - 1]})
        $calcTotal && total=$(math::add $total ${square[i]})
    done
}

bash_only::atSquare() {
    local -a square
    bash_only::calcSquares "$1"
    echo ${square[$1]}
}

bash_only::total() {
    local total
    local -a square
    bash_only::calcSquares -T 64
    echo $total
}
