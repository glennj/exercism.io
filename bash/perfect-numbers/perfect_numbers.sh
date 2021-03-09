#!/usr/bin/env bash

aliquot_sum() {
    local -i num=$1 sum

    # So we don't need any special cases for num==1, use
    # a Set to store the factors, then remove $num from the set.
    #
    # Use the indices of an array as the set.

    local factors=()
    for ((i = 1; i * i <= num; i++)); do
        if ((num % i == 0)); then
            factors[i]=1
            factors[num / i]=1
        fi
    done
    unset "factors[num]"

    sum=0
    for factor in "${!factors[@]}"; do
        ((sum += factor))
    done

    echo "$sum"
}

main() {
    local -i num=$1

    if ((num <= 0)); then
        echo "Classification is only possible for natural numbers." >&2
        exit 1
    fi

    sum=$(aliquot_sum "$num")

    if   ((sum < num)); then echo deficient
    elif ((sum > num)); then echo abundant
    else                     echo perfect
    fi
}

main "$@"
