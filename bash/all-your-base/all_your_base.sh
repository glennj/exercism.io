#!/bin/bash

source ./utils.bash    # `assert`

to_decimal() {
    local -i from_base=$1 decimal=0 digit

    for digit in "${@:2}"; do
        assert "digit < from_base" "Digit must be less than the from base"
        assert "digit >= 0"        "Digit cannot be negative"

        ((decimal = from_base * decimal + digit))
    done
    echo "$decimal"
}

to_base() {
    local -i to_base=$1 decimal=$2
    local -a digits

    while ((decimal > 0)); do
        ((digit = decimal % to_base))
        ((decimal /= to_base))
        digits=("$digit" "${digits[@]}")
    done
    echo "${digits[*]}"
}

main() {
    local -i from_base=$1
    local    from_digits=$2
    local -i to_base=$3

    assert "from_base > 1" "From base must be greater than 1"
    assert "to_base   > 1" "To base must be greater than 1"

    local -i decimal
    local -a digits
    read -ra digits <<< "$from_digits"

    decimal=$(to_decimal "$from_base" "${digits[@]}") || exit $?

    to_base "$to_base" "$decimal"
}

main "$@"
