#!/bin/bash

shopt -s extglob

main() {
    if (( $# != 1 )); then
        echo "usage: ${0##*/} ISBN-number" >&2
        exit 1
    fi
    isbn10 "$1" && echo true || echo false
}

isbn10() {
    # validate length
    local input=${1//-/}
    (( ${#input} == 10 )) || return 1

    # validate first 9 chars are all digits
    local first9=${input:0:9}
    [[ $first9 == +([0-9]) ]] || return 1

    # validate check character
    local check=${input: -1:1}
    [[ $check == [0-9X] ]]  || return 1

    # confirm the given check digit is correct
    [[ $check == "$(isbn10_check_digit $first9)" ]]
}

isbn10_check_digit() {
    local -i i sum=0 check
    for ((i=0; i<9; i++)); do
        (( sum += ${1:i:1} * (10-i) ))
    done
    check=$(( 11 - sum % 11 ))
    (( $check == 10 )) && echo X || echo $check
}

main "$@"
