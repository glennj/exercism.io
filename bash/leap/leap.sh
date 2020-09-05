#!/usr/bin/env bash

if [[ $# -ne 1 || $1 == *[^[:digit:]]* ]]; then
    echo "Usage: $(basename "$0") <year>" >&2
    exit 1
fi

isLeap() {
    (( $1 % 4 == 0 && ($1 % 100 != 0 || $1 % 400 == 0) ))
}

isLeap "$1" && echo true || echo false
