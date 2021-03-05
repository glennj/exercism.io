#!/usr/bin/env bash

source ../lib/utils.bash

luhn() {
    local num=$1
    local -i i d digit sum=0

    # x = n * 2; if x > 9 then x -= 9
    local -a double=(0 2 4 6 8 1 3 5 7 9)

    for ((i = ${#num} - 1, d = 0; i >= 0; i--, d = !d)); do
        digit=${num:i:1}
        ((sum += (d ? double[digit] : digit)))
    done

    ((sum % 10 == 0))
}

main() {
    assert "$# == 1" "usage: ${0##*/} cc_number"
    local cc=${1//[[:space:]]/}
    if [[ ${#cc} -lt 2 || $cc =~ [^[:digit:]] ]]; then
        # invalid number: too short or non-digits
        echo false
    else
        luhn "$cc"
        true_or_false
    fi
}

main "$@"
