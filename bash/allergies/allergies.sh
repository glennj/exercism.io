#!/bin/bash

source ../lib/utils.bash
checkBashVersion 4.0 "array concatenation bug fix"

main() {
    validate "$@"

    local allergies=()
    getAllergies "$1" ;# populates allergies array

    case "$2" in
        list) 
            echo "${allergies[*]}"
            ;;
        allergic_to)
            for a in "${allergies[@]}"; do
                if [[ $a == "$3" ]]; then
                    echo true
                    return
                fi
            done
            echo false
            ;;
    esac
}

validate() {
    (( $# < 2 )) && usage
    [[ $2 != @(list|allergic_to) ]] && usage
    [[ $2 == "allergic_to" ]] && [[ -z $3 ]] && usage
}

usage() {
    echo "usage: ${0##*/} <code> list" >&2
    echo "usage: ${0##*/} <code> allergic_to <allergen>" >&2
    exit 255
}

getAllergies() {
    local code=$1
    local allergens=(
        eggs peanuts shellfish strawberries
        tomatoes chocolate pollen cats
    )

    for (( i=0; i<${#allergens[@]}; i++ )); do
        if (( (code & (1 << i)) != 0 )); then
            allergies+=("${allergens[i]}")
        fi
    done
}

main "$@"
