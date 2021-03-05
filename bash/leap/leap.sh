#!/usr/bin/env bash

isLeap() {
    (($1 % 4 == 0 && ($1 % 100 != 0 || $1 % 400 == 0)))
}

# convert the _last_ exit status to a string
true_or_false() {
    # shellcheck disable=SC2181
    (($? == 0)) && echo true || echo false
}

main() {
    if [[ $# -ne 1 || $1 =~ [^[:digit:]] ]]; then
        echo "Usage: ${0##*/} <year>" >&2
        exit 1
    fi
    isLeap "$1"
    true_or_false
}

main "$@"
