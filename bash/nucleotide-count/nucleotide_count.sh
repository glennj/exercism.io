#!/bin/bash

# shellcheck disable=SC2034,SC2219

source ./utils.bash

count() {
    local A=0 C=0 G=0 T=0

    while IFS= read -r -n1 c; do
        case $c in
            A|C|G|T) let "$c++" ;;
            *) die "Invalid nucleotide in strand" ;;
        esac
    done < <(printf "%s" "$1")

    for n in A C G T; do
        echo "$n: ${!n}"
    done
}

count "$@"
