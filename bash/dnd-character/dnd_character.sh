#!/usr/bin/env bash

characteristics=(
    strength
    dexterity
    constitution
    intelligence
    wisdom
    charisma
)

main() {
    case $1 in
        modifier|generate) "$@" ;;
        *) echo "unknown subcommand" >&2; exit 1 ;;
    esac
}

modifier() {
    local -i n=$1
    # a little trickery to ensure we round *down*
    (( n < 10 )) && (( n-- ))
    echo $(( (n - 10) / 2 ))
}

generate() {
    local a c hp
    for c in "${characteristics[@]}"; do
        a=$(ability)
        [[ $c == constitution ]] && hp=$(( 10 + $(modifier $a) ))
        echo "$c $a"
    done
    echo "hitpoints $hp"
}

ability() {
    # using sort: add the top 3 rolls
    #set -- $( { d6; d6; d6; d6; } | sort -nr )
    #echo $(( $1 + $2 + $3 ))
    
    # add 4 dice rolls, then subtract the smallest one
    # Thanks to @ScoobD for the inspiration
    local roll sum min
    for _ in {1..4}; do
        roll=$(d6)
        (( roll < ${min:-7} )) && min=$roll
        (( sum += roll ))
    done
    echo $(( sum - min ))
}

# roll a 6-sided die
d6() { echo $(( 1 + RANDOM % 6 )); }

main "$@"
