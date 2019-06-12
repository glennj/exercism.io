#!/usr/bin/env bash

assert() {
    if [[ $1 -eq 0 ]]; then
        echo "$2" >&2
        exit 1
    fi
}

series=$1
len=${#series}
sequences=()
declare -i slice=$2

assert $(( slice != 0   )) "slice length cannot be zero"
assert $(( slice >= 0   )) "slice length cannot be negative"
assert $(( len > 0      )) "series cannot be empty"
assert $(( len >= slice )) "slice length cannot be greater than series length"

for ((i=0; i <= len - slice; i++)); do
    sequences+=( "${series:i:slice}" )
done

echo "${sequences[*]}"
