#!/usr/bin/env bash

source ../lib/utils.bash
source ../lib/utils_math.bash

validate() {
    local player=$1
    # shellcheck disable=SC2034
    local -i row=$2 col=$3

    assert "row >= 0" "$player row not positive"
    assert "row <= 7" "$player row not on board"
    assert "col >= 0" "$player column not positive"
    assert "col <= 7" "$player column not on board"
}

main() {
    local -i blackRow blackCol whiteRow whiteCol

    while getopts :b:w: opt; do
        case $opt in
            b) IFS=, read -r blackRow blackCol <<< "$OPTARG" ;;
            w) IFS=, read -r whiteRow whiteCol <<< "$OPTARG" ;;
            :) die "Missing argument for option -$OPTARG" ;;
            *) : ;;
        esac
    done

    validate black "$blackRow" "$blackCol"
    validate white "$whiteRow" "$whiteCol"
    refute "blackRow == whiteRow && blackCol == whiteCol" \
        "cannot occupy same position"

    local -i rowDelta colDelta
    rowDelta=$(math::abs $((blackRow - whiteRow)))
    colDelta=$(math::abs $((blackCol - whiteCol)))

    ((rowDelta == 0 || colDelta == 0 || rowDelta == colDelta))
    true_or_false
}

main "$@"
