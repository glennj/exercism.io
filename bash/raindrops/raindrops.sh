#!/bin/bash
#
# extracting the list of indices for an indexed array
# returns the indices numerically sorted.
# https://stackoverflow.com/q/57185987/7552

[[ $# -ne 1 || $1 != +([0-9]) ]] && exit 1

sounds=( [3]=Pling [5]=Plang [7]=Plong )

# zero has no factors
if (( $1 > 0 )); then
    for i in "${!sounds[@]}"; do
        (( $1 % i == 0 )) && output+=${sounds[i]}
    done
fi

echo "${output:-$1}"
