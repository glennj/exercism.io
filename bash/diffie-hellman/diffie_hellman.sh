#!/usr/bin/env bash

# Input gets validate as ints, quoting is not a problem
# shellcheck disable=SC2086

source ../lib/utils.bash
source ../lib/utils_math.bash
source ../lib/utils_string.bash

privateKey() { math::rand 2 $1; }
publicKey()  { powmod $2 $3 $1; }
secret()     { publicKey "$@"; }

# function to do `pow(a, b) % c`
powmod() {
    local -i a=$1 b=$2 c=$3
    local -i result=1 i
    for ((i = 1; i <= b; i++)); do
        ((result = (result * a) % c))
    done
    echo $result
}

assertInts() {
    for arg; do
        assert -C str::isInt "$arg" "not an integer: $arg"
    done
}

main() {
    case $1 in
        privateKey | publicKey | secret)
            assertInts "${@:2}"
            "$1" "${@:2}"
            ;;
        *)  die "unknown subcommand" ;;
    esac
}

main "$@"
