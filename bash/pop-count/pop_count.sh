#!/usr/bin/env bash
#
# The `|| :` are meant to protect this script when running with `bash -e`

countBits() {
    local number=$1 count
    while ((number > 0)); do
        ((number & 1)) && ((count++)) || :
        ((number >>= 1)) || :
    done
    printf '%d\n' "$count"
}

countBits "$1"
