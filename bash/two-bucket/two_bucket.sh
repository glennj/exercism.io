#!/usr/bin/env bash

main() {
    local -A b1=([name]=one [size]=0 [amount]=0)
    local -A b2=([name]=two [size]=0 [amount]=0)
    b1[size]=$1
    b2[size]=$2
    local goal=$3 start=$4

    validate ${b1[size]} ${b2[size]} $goal

    case $start in
        one) solve b1 b2 $goal ;;
        two) solve b2 b1 $goal ;;
        *) echo "invalid start bucket" >&2; exit 1 ;;
    esac
}

validate() {
    local a=$1 b=$2 goal=$3

    # the goal amount must fit in a single bucket
    local max; (( a > b )) && max=$a || max=$b
    if (( goal > max )); then
        echo "invalid goal: too big" >&2
        exit 1
    fi

    # if the buckets are not relatively prime, then
    # the goal must be divisible by the greatest
    # common divisor of the buckdets
    local gcd=$(gcd $a $b)
    if (( gcd > 1 && (goal % gcd) != 0 )); then
        echo "invalid goal: unsatisfiable" >&2
        exit 1
    fi
}

gcd() {
    local -i a=$1 b=$2
    if (( b > 0 )); then
        gcd $b $((a % b))
    else
        echo $a
    fi
}

solve() {
    local -n start=$1 other=$2
    local -i goal=$3 moves

    fill start
    moves=1

    ((other[size] == goal)) && {
        fill other
        (( moves++ ))
    }

    while true; do
        if (( start[amount] == goal )); then
            output $moves ${start[name]} ${other[amount]}
            return
        elif (( other[amount] == goal )); then
            output $moves ${other[name]} ${start[amount]}
            return
        fi

        # heh, some (mostly) punctuation-free bash poetry
        if is empty start; then fill start
        elif is full other; then empty other
        else pour from start to other
        fi

        (( moves++ ))
    done
}

output() {
    echo "moves: $1, goalBucket: $2, otherBucket: $3"
}

is() {
    local -n bucket=$2
    case $1 in
        full)  (( bucket[amount] == bucket[size] )) ;;
        empty) (( bucket[amount] == 0 )) ;;
    esac
}

capacity() {
    local -n bucket=$1
    echo $(( bucket[size] - bucket[amount] ))
}

fill() {
    local -n bucket=$1
    bucket[amount]=${bucket[size]}
}

empty() {
    local -n bucket=$1
    bucket[amount]=0
}

pour() {
    # have to be careful not to reuse a variable name from
    # anywhere up the stack.
    local -n _b1=$2 _b2=$4
    local b1_amt=${_b1[amount]}
    local b2_amt=${_b2[amount]} 
    local b2_cap=$(capacity _b2)

    # determine the amount to pour: minimum of
    # b1's current amount and b2's current capacity
    local amount
    (( b1_amt < b2_cap )) && amount=$b1_amt || amount=$b2_cap
    _b1[amount]=$(( b1_amt - amount ))
    _b2[amount]=$(( b2_amt + amount ))
}

main "$@"
