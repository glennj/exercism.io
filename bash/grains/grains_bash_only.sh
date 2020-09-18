#!/usr/bin/env bash
#
# A version with no external tools used.
#
# I do not recommend this approach if you care about performance.

source ../lib/utils.bash        # `die`
source ../lib/utils_math.bash   # `math::add`

shopt -s extglob

# populate the grains in the necessary squares
calcSquares() {
    local OPTIND calcTotal=false
    while getopts "T" opt; do
        [[ $opt == T ]] && calcTotal=true
    done
    shift $((OPTIND - 1))

    square=(0 1)
    $calcTotal && total=${square[1]}

    for (( i = 2; i <= $1; i++ )); do
        square[i]=$(math::add ${square[i-1]} ${square[i-1]})
        $calcTotal && total=$(math::add $total ${square[i]})
    done
}

main() {
    local -a square
    local total
    local result=0

    case $1 in
        [1-9]*([0-9]))
            (($1 <= 64)) || die "Error: invalid input"
            calcSquares $1
            echo ${square[$1]}
            ;;
        total)
            calcSquares -T 64
            echo $total
            ;;
        *)
            die "Error: invalid input"
            ;;
    esac
}

main "$@"
