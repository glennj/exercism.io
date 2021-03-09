#!/usr/bin/env bash

source ../lib/utils.bash
checkBashVersion 4.3 "associative array index fixes"

if [[ -t 0 ]]; then
    # No input redirection, stdin is a tty: handle this as a
    # "no input" situation, do not wait for user input.
    lines=()
else
    # read data from stdin into an array
    readarray -t lines
fi

assert "${#lines[@]} % 4 == 0" "Number of input lines is not a multiple of four"

for line in "${lines[@]}"; do
    assert "${#line} % 3 == 0" "Number of input columns is not a multiple of three"
done

declare -A map=(
    [" _ | ||_|   "]=0
    ["     |  |   "]=1
    [" _  _||_    "]=2
    [" _  _| _|   "]=3
    ["   |_|  |   "]=4
    [" _ |_  _|   "]=5
    [" _ |_ |_|   "]=6
    [" _   |  |   "]=7
    [" _ |_||_|   "]=8
    [" _ |_| _|   "]=9
)

output=""
separator=","

for ((i = 0; i < ${#lines[@]} / 4; i++)); do
    ((i > 0)) && output+=$separator

    for ((j = 0; j < ${#lines[0]}; j += 3)); do
        ocr_digit=""
        for k in {0..3}; do
            ocr_digit+=${lines[i * 4 + k]:j:3}
        done
        output+=${map[$ocr_digit]:-"?"}
    done

done

echo "$output"
