#!/bin/bash

using_array() {
    # extracting the list of indices for an indexed array
    # returns the indices numerically sorted.
    # => https://stackoverflow.com/q/57185987/7552

    local -r sounds=([3]=Pling [5]=Plang [7]=Plong)
    local output

    # zero has no factors
    if (($1 > 0)); then
        for i in "${!sounds[@]}"; do
            (($1 % i == 0)) && output+=${sounds[i]}
        done
    fi

    echo "${output:-$1}"
}

using_indirect_variables() {
    # shellcheck disable=SC2034
    local Pling=3 Plang=5 Plong=7
    local output

    if (($1 > 0)); then
        for var in Pling Plang Plong; do
            (($1 % ${!var} == 0)) && output+=$var
        done
    fi

    echo "${output:-$1}"
}

# extended patterns are enabled inside [[...]]
[[ $# -ne 1 || $1 != +([0-9]) ]] && exit 1

##using_array "$1"
using_indirect_variables "$1"
