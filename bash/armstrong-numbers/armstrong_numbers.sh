#!/usr/bin/env bash

is_armstrong() {
    local -i num=$1 sum=0 digit
    local -i len=${#num}

    while read -r -n1 digit; do
        ((sum += digit ** len))
    done < <(printf '%s' "$num")
    ((sum == num))
}

is_armstrong "$1" && echo true || echo false
