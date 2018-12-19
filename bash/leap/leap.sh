#!/bin/bash
shopt -s extglob

if [[ $# -ne 1 || $1 != +([[:digit:]]) ]]; then
    echo "Usage: $(basename "$0") <year>" >&2
    exit 1
fi

if (( $1 % 400 == 0 || ($1 % 4 == 0 && $1 % 100 != 0) )); then
    echo true
else
    echo false
fi
