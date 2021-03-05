#!/usr/bin/env bash

source ../lib/utils.bash
checkBashVersion 4.3 namerefs

main() {
    local -i amount=$(($1))
    # shellcheck disable=SC2034
    local -ia denonimations=("${@:2}")
    # shellcheck disable=SC2034
    local -a firstCoinForValue=() # not given integer attribute

    if ((amount < 0)); then
        echo "target can't be negative" >&2
        exit 1
    elif ((amount > 0)); then
        # Passing 2 arrays by nameref
        # Populates the `firstCoinForValue` array
        change $amount denonimations firstCoinForValue
        makeChange $amount denonimations firstCoinForValue
    fi
}

#   Change making algorithm from
#   http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf
#
#   This function generates two arrays:
#
#   C = maps the minimum number of coins required to make
#       change for each n from 1 to amount.  It is only
#       used internally in this function.
#
#   S = the _first_ coin used to make change for amount n
#       (actually stores the coin _index_ into the coins array)

change() {
    local -i amount=$1
    local -n coins=$2
    local -n S=$3
    local -a C
    local -i max=99999999 min i p
    local coin # not an integer variable

    C=(0)
    S=(0)
    for ((i = 1; i <= amount; i++)); do
        C+=("$max")
        S+=("")
    done

    for ((p = 1; p <= amount; p++)); do
        min=$max
        coin=""
        for ((i = 0; i < ${#coins[@]}; i++)); do
            if ((coins[i] <= p)); then
                if ((1 + C[p - coins[i]] < min)); then
                    min=$((1 + ${C[p - coins[i]]}))
                    coin=$i
                fi
            fi
        done
        C[p]=$min
        S[p]=$coin
    done
}

makeChange() {
    local -i amount=$1
    local -n coins=$2
    local -n firstCoin=$3
    local -ia change
    local -i coin idx

    if [[ -z ${firstCoin[amount]} ]]; then
        echo "can't make target with given coins" >&2
        return 1
    fi

    while ((amount > 0)); do
        idx=${firstCoin[amount]}
        coin=${coins[idx]}
        # shellcheck disable=SC2206
        change+=($coin)
        ((amount -= coin))
    done

    printf "%d\n" "${change[@]}" | sort -n | paste -sd" "
}

main "$@"
