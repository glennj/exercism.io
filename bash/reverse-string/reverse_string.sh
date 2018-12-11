#!/usr/bin/env bash

reverse () {
    # cheating
    #rev <<< "$1"

    # implement in bash
    local reversed=""
    local -i i
    for (( i=${#1}-1; i >= 0; i-- )); do
        reversed+="${1:i:1}"
    done
    echo "$reversed"
}

reverse "$*"
