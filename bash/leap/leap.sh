#!/usr/bin/env bash

isLeap() {
    (($1 % 4 == 0 && ($1 % 100 != 0 || $1 % 400 == 0)))
}

if [[ $# -ne 1 || -z $1 || $1 =~ [^[:digit:]] ]]; then
    echo "Usage: ${0##*/} <year>" >&2
    exit 1
fi

isLeap "$1" && echo true || echo false
