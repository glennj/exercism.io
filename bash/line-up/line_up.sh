#!/usr/bin/env bash

function ordinal {
    local ones=$(( $1 % 10 ))
    local tens=$(( $1 % 100 ))
    if (( 11 <= tens && tens <= 13 )); then echo "th"
    elif (( ones == 1 )); then echo "st"
    elif (( ones == 2 )); then echo "nd"
    elif (( ones == 3 )); then echo "rd"
    else echo "th"
    fi
}

set -- "$1" "$2" "$(ordinal "$2")"
printf '%s, you are the %d%s customer we serve today. Thank you!\n' "$@"
