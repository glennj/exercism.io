#!/usr/bin/env bash

# namerefs introduced in bash 4.3
if    [[ ${BASH_VERSINFO[0]} -lt 4 ]] ||
    { [[ ${BASH_VERSINFO[0]} -eq 4 ]] && [[ ${BASH_VERSINFO[1]} -lt 3 ]]; }
then
    echo "bash version 4.3 required" >&2
    exit 2
fi

if [[ -t 0 ]]; then
    # no redirected input
    exit
fi

main() {
    local -a input output
    local -i i j

    readarray -t input
    pad_lines input

    output=()
    for (( i=0; i < ${#input[@]}; i++ )); do
        for (( j=0; j < ${#input[i]}; j++ )); do
            output[j]+=${input[i]:j:1}
        done
    done

    printf "%s\n" "${output[@]}"
}

# Starting from the 2nd-last line and going backwards,
# each line in the array must be at least as long as the
# *following* line
pad_lines() {
    local -n ary=$1
    local -i i
    for (( i=${#ary[@]} - 2; i >= 0; i-- )); do
        # Pad the right end of the string with spaces
        ary[i]=$(printf "%-*s" ${#ary[i+1]} "${ary[i]}")
    done
}

main
