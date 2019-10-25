#!/bin/bash

main() {
    case $1 in
        square_of_sum | sum_of_squares | difference) "$@" ;;
        *) echo "unknown subcommand $1" >&2 ; exit 1 ;;
    esac
}

square_of_sum() {
    local n=$1 sum=0 i
    for ((i=1; i<=n; i++)); do ((sum += i)); done
    echo $((sum ** 2))
}

sum_of_squares() {
    local n=$1 sum=0 i
    for ((i=1; i<=n; i++)); do ((sum += (i ** 2))); done
    echo $sum
}

difference() {
    abs $(( $(sum_of_squares $1) - $(square_of_sum $1) ))
}

abs() {
    echo $(( $1 < 0 ? -1 * $1 : $1 ))
}

main "$@"
