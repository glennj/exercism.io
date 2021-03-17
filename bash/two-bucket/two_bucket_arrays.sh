#!/usr/bin/env bash

# Shellcheck throws plenty of warnings, none to worry about

source ../lib/utils.bash
source ../lib/utils_math.bash

checkBashVersion 4.3 namerefs

solve() {
    local -n first=$1 second=$2
    local -i goal=$3 moves=0

    fill first
    moves+=1    # -i attribute allows "bare" arithmetic

    if ((second[size] == goal)); then
        fill second
        moves+=1
    fi

    while true; do
        satisfied $goal first second $moves && return
        satisfied $goal second first $moves && return

        if      is empty first
        then    fill first

        elif    is full second
        then    empty second

        else    pour from first to second
        fi

        moves+=1
    done
}

satisfied() {
    local -n a=$2 b=$3
    local -i goal=$1 moves=$4
    ((a[amount] == goal)) || return 1
    echo "moves: $moves," \
         "goalBucket: ${a[name]}," \
         "otherBucket: ${b[amount]}"
}

is() {
    local -n a=$2
    case $1 in
        full)  ((a[amount] == a[size])) ;;
        empty) ((a[amount] == 0)) ;;
    esac
}

fill() {
    local -n a=$1
    a[amount]=${a[size]}
}

empty() {
    local -n a=$1
    a[amount]=0
}

pour() {
    local -n a=$2 b=$4
    local -i amount

    # determine the amount to pour: minimum of
    # a's current amount and b's current free space
    amount=$(math::min ${a[amount]} $((b[size] - b[amount])))

    ((a[amount] -= amount))
    ((b[amount] += amount))
}

############################################################
validate() {
    local -i a=$1 b=$2 goal=$3

    # the goal amount must fit in a single bucket
    local -i max=$(math::max $a $b)
    assert "goal <= max" "invalid goal: too big"

    # if the buckets are not relatively prime, then
    # the goal must be divisible by the greatest
    # common divisor of the buckdets
    local -i gcd=$(math::gcd $a $b)
    assert "gcd == 1 || (goal % gcd) == 0" "invalid goal: unsatisfiable"
}

############################################################
main() {
    validate "${@:1:3}"

    local -A b1=([name]=one [size]=$1 [amount]=0)
    local -A b2=([name]=two [size]=$2 [amount]=0)

    case $4 in
        one) solve b1 b2 "$3" ;;
        two) solve b2 b1 "$3" ;;
        *)   die "invalid start bucket" ;;
    esac
}

main "$@"
