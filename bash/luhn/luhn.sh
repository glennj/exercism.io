#!/bin/bash

source ../lib/utils_string.bash

main() {
    if (( $# != 1 )); then
        echo "usage: $(basename "$0") cc_number" >&2
        exit 1
    fi

    local cc=${1//[[:space:]]/}   # strip all whitespace

    if [[ ${#cc} -lt 2 || $cc == *[^[:digit:]]* ]]; then
        # invalid number: too short or non-digits
        echo false
        exit 0
    fi

    luhn "$cc" && echo true || echo false
}

luhn() {
    local num=$(str::reverse "$1")
    local -i i digit sum=0

    # x = n * 2; if x > 10 then x -= 9
    local -a double=(0 2 4 6 8 1 3 5 7 9)

    for ((i=0; i<${#num}; i++)); do
        digit=${num:i:1}
        (( i%2 == 1 )) && digit=${double[digit]}
        ((sum += digit))
    done
    (( sum % 10 == 0 )) 
}

main "$@"
