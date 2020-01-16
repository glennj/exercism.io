#!/bin/bash
#
# with no external tools used

shopt -s extglob

main() {
    local -a square
    local result=0

    case $1 in
        [1-9]*([0-9]))
            (($1 <= 64)) || die "invalid input"
            calcSquares $1
            echo ${square[$1]}
            ;;
        total)
            calcSquares 64
            local total=0
            for s in "${square[@]}"; do
                total=$(add $total $s)
            done
            echo $total
            ;;
        *)
            die "invalid input"
            ;;
    esac
}

die() { echo "Error: $*" >&2; exit 1; }

# populate the grains in the necessary squares
calcSquares() {
    square=(0 1)
    for (( i = 2; i <= $1; i++ )); do
        square[i]=$(add ${square[i-1]} ${square[i-1]})
    done
}

# Add two arbitrarily large integers.
# We'll use string manipulation to avoid integer overflow.
add() {
    local a=$1 b=$2 c
    local result="" carry=0

    # left pad the numbers with zeroes so they're the same width
    local width
    (( ${#a} > ${#b} )) && width=${#a} || width=${#b}
    printf -v a '%0*s' $width $a
    printf -v b '%0*s' $width $b

    # add the digits from right to left
    for (( i = width-1; i >= 0; i-- )); do
        (( c = carry + ${a:i:1} + ${b:i:1} ))
        result=$((c % 10))${result}
        (( carry = c / 10 ))
    done
    result=${carry}${result}

    echo ${result##+(0)}
}

main "$@"
