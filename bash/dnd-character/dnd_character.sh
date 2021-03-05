#!/usr/bin/env bash

readonly characteristics=(
    strength
    dexterity
    constitution
    intelligence
    wisdom
    charisma
)

modifier() {
    local -i n=$1
    # a little trickery to ensure we round *down*
    ((n < 10)) && ((n--))
    echo $(((n - 10) / 2))
}

generate() {
    local a c
    for c in "${characteristics[@]}"; do
        a=$(ability)
        echo "$c $a"
        if [[ $c == constitution ]]; then
            echo "hitpoints $((10 + $(modifier "$a")))"
        fi
    done
}

ability() {
    # add 4 dice rolls, then subtract the smallest one
    # Thanks to @ScoobD for the inspiration
    local roll sum min=6
    for _ in {1..4}; do
        roll=$(d6)
        ((roll < min)) && min=$roll
        ((sum += roll))
    done
    echo $((sum - min))
}

# roll a 6-sided die
d6() { echo $((1 + RANDOM % 6)); }

main() {
    case $1 in
        modifier | generate) "$@" ;;
        *) echo "unknown subcommand" >&2; exit 1 ;;
    esac
}

main "$@"
