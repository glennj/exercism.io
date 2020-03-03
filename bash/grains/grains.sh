#!/bin/bash
#
# with no external tools used

source ../lib/utils.bash        # `die`
source ../lib/utils_math.bash   # `math::add`

shopt -s extglob

main() {
    local -a square
    local result=0

    case $1 in
        [1-9]*([0-9]))
            (($1 <= 64)) || die "Error: invalid input"
            calcSquares $1
            echo ${square[$1]}
            ;;
        total)
            calcSquares 64
            local total=0
            for s in "${square[@]}"; do
                total=$(math::add $total $s)
            done
            echo $total
            ;;
        *)
            die "Error: invalid input"
            ;;
    esac
}

# populate the grains in the necessary squares
calcSquares() {
    square=(0 1)
    for (( i = 2; i <= $1; i++ )); do
        square[i]=$(math::add ${square[i-1]} ${square[i-1]})
    done
}

main "$@"
