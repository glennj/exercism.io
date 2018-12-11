#!/bin/bash

main() {
    if (( $# != 2 )); then
        echo "usage: $(basename "$0") <encode|decode> string" >&2
        exit 1
    fi

    # alphabets
    plain=abcdefghijklmnopqrstuvwxyz
    cipher=zyxwvutsrqponmlkjihgfedcba

    case "$1" in
        encode|decode) "$1" "$2" ;;
        *) echo "error: unknown subcommand '$1'" >&2; exit 1 ;;
    esac
}

encode() { x_code "$1" | groups; }
decode() { x_code "$1"; }

x_code() { echo "$1" | lower_case | alnum_only | encipher; }

lower_case() { tr '[:upper:]' '[:lower:]'; }
alnum_only() { tr -dc "$plain[:digit:]"; }
encipher()   { tr "$plain" "$cipher"; }
groups()     { sed -Ee "s/.{${1:-5}}/& /g" -e 's/ $//'; }

main "$@"
