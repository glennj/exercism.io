#!/usr/bin/env bash

# works with bash v3.2.57

source ./utils.bash     # for `die`, `assert`

readonly SHIFT=7
readonly MSB=$((  2#10000000 ))   # "most significant bit"
readonly MASK=$(( 2#01111111 ))

# convert a base-16 number to base 10
# ex:
#   $ hex2dec 2F
#   47
hex2dec() { printf "%lu" "0x${1}"; }

# convert a base-10 number to base-16
# ex:
#   $ dec2hex 47
#   2F
dec2hex() { printf "%02X" "$1"; }

encode() {
    local result value val bytes msb
    result=()

    for value in "$@"; do
        val=$(hex2dec "$value")
        bytes=()
        msb=0
        while true; do
            byte=$(dec2hex $(((val & MASK) | msb)))
            bytes=("$byte" "${bytes[@]}")
            ((val >>= SHIFT))
            ((val == 0)) && break
            msb=$MSB
        done
        result+=("${bytes[@]}")
    done

    echo "${result[*]}"
}

decode() {
    local values n byte

    values=()
    n=0
    for byte in "$@"; do
        val=$(hex2dec "$byte")
        n=$(((n << SHIFT) + (val & MASK)))
        if (((val & MSB) == 0)); then
            values+=("$(dec2hex $n)")
            n=0
        fi
    done

    (((val & MSB) == 0)) || die "incomplete byte sequence"
    echo "${values[*]}"
}

case $1 in
    encode | decode) "$1" "${@:2}" ;;
    *) die "unknown subcommand" ;;
esac
