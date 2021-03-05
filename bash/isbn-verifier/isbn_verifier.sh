#!/usr/bin/env bash

isbn10() {
    local input=${1//-/}

    # validate content
    [[ $input =~ ^[0-9]{9}[0-9X]$ ]] || return 1

    # validate check digit
    local -i i sum=0
    for ((i = 0; i < 9; i++)); do
        ((sum += ${input:i:1} * (10 - i)))
    done

    local check=${input: -1:1}
    (((11 - sum % 11) == (check == "X" ? 10 : check)))
}

main() {
    if (($# != 1)); then
        echo "usage: ${0##*/} ISBN-number" >&2
        exit 1
    fi
    isbn10 "$1" && echo true || echo false
}

main "$@"
