#!/bin/bash

source ../lib/utils.bash
checkBashVersion 4.0 "lower-casing parameter expansion"

############################################################
decode() { <<< "$1" alnum_only | lower_case | encipher; }
encode() { decode "$1" | chunked 5; }

############################################################
# These are nice one line functions that I would use in
# a production script.

#    alnum_only() { tr -dc '[:alnum:]'; }
#    lower_case() { tr '[:upper:]' '[:lower:]'; }
#    encipher()   { tr '[:lower:]' 'zyxwvutsrqponmlkjihgfedcba'; }
#    chunked()    { sed -Ee "s/.{${1:-5}}/& /g" -e 's/ $//'; }

# Since I'm emulating external utilities in plain bash,
# make these functions behave silimarly: iterate over 
# all the lines that are fed to them.

# The default varname for `read` is `$REPLY`

alnum_only() {
    while IFS= read -r; do
        printf '%s\n' "${REPLY//[^[:alnum:]]/}"
    done
}

lower_case() {
    while IFS= read -r; do
        printf '%s\n' "${REPLY,,}"
    done
}

encipher() {
    local result char

    while IFS= read -r; do
        result=""

        # a while-read loop to access the characters
        # of a string is faster than a for loop.
        while IFS= read -r -d "" -n1 -u3 char; do
            result+=$(cipherChar "$char")
        done 3< <(printf '%s' "$REPLY")

        printf '%s\n' "$result"
    done
}

readonly Alphabet=$( set -- {a..z}; IFS=""; echo "$*" )

# Map a to z, b to y, ...
#
# Suppose $1 is:        X     9
# * $char is:           x     9
# * $suffix is:         yz    abcdefghijklmnopqrstuvwxyz
# * ${#suffix} is:      2     26
# * ${Alphabet:n:1} is: c     ""

cipherChar() {
    local char=${1,}
    local suffix=${Alphabet#*$char}
    local enciphered=${Alphabet: ${#suffix} :1}
    printf '%s\n' "${enciphered:-$char}"
}

chunked() {
    local -i size=${1:-5} len i
    local chunks
    while IFS= read -r; do
        len=${#REPLY}
        chunks=${REPLY:0:size}
        for (( i=size; i < len; i += size )); do
            chunks+=" ${REPLY:i:size}"
        done
        printf '%s\n' "$chunks"
    done
}

############################################################
main() {
    assert "$# == 2" "usage: ${0##*/} <encode|decode> string"

    case "$1" in
        encode|decode) "$1" "$2" ;;
        *) die "error: unknown subcommand '$1'" ;;
    esac
}

main "$@"
