#!/bin/bash

luhn() {
    local num=$(reverse "$1")
    # "0" is invalid, but "00" is. go figure.
    [[ $num == "0" ]] && return 1
    local -i i digit sum=0
    for ((i=0; i<${#num}; i++)); do
        digit=${num:i:1}
        if (( i%2 == 1 )); then
            ((digit *= 2))
            ((digit > 9)) && ((digit -= 9))
        fi
        ((sum += digit))
    done
    (( sum % 10 == 0 )) 
}

reverse () {
    local reversed=""
    local -i i
    for (( i=${#1}-1; i >= 0; i-- )); do
        reversed+="${1:i:1}"
    done
    echo "$reversed"
}

if (( $# != 1 )); then
    echo "usage: $(basename "$0") cc_number" >&2
    exit 1
fi

# strip all whitespace
cc=${1//[[:space:]]/}

if [[ $cc == *[^[:digit:]]* ]]; then
    # invalid number: non-digits
    echo false
    exit 0
fi

luhn "$cc" && echo true || echo false
