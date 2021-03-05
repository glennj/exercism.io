#!/bin/bash

source ../lib/utils.bash    # `assert`

main() {
    local -i from_base=$1
    local    from_digits=$2
    local -i to_base=$3

    assert "from_base > 1" "From base must be greater than 1"
    assert "to_base   > 1" "To base must be greater than 1"

    local digits
    read -ra digits <<< "$from_digits"

    local -i decimal=0
    for digit in "${digits[@]}"; do
        assert "digit < from_base" "Digit must be less than the from base"
        assert "digit >= 0"        "Digit cannot be negative"
        ((decimal = from_base * decimal + digit))
    done

    digits=()
    while ((decimal > 0)); do
        ((digit = decimal % to_base))
        ((decimal /= to_base))
        digits=("$digit" "${digits[@]}")
    done

    echo "${digits[*]}"
}

main "$@"
