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

#######################################################################
generate() {
    # ugly variable name: shellcheck has some bugs with re-using varnames,
    # even in different functions. https://www.shellcheck.net/wiki/SC2178
    local -n _g_character=$1
    local c
    for c in "${characteristics[@]}"; do
        _g_character[$c]=$(ability)
        if [[ $c == constitution ]]; then
            _g_character["hitpoints"]=$((10 + $(modifier "${_g_character[$c]}")))
        fi
    done
}

display() {
    local -n _d_character=$1
    local c
    for c in "${!_d_character[@]}"; do
        printf '%s %s\n' "$c" "${_d_character[$c]}"
    done
}

#######################################################################
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

die() { echo "$*" >&2; exit 1; }

#######################################################################
main() {
    case $1 in
        modifier) $1 "$2" ;;
        generate)
            # shellcheck disable=SC2034
            # the variable is *not* unused
            local -A character
            # separating the _creation_ of a character from the _display_ of a character
            generate character
            display character
            ;;
        *) die "unknown subcommand" ;;
    esac
}

main "$@"
