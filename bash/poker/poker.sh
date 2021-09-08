#!/usr/bin/bash

# external tools used: sort

source ./utils.bash
source ./utils_math.bash
checkBashVersion 4.0 "associative arrays"

declare -ri SUCCESS=0 FAILURE=1

# Assign a numeric value to this hand.
#
# This is trickier than first glance. We can say that
# a 5-of-a-kind hand has the "highest value", but two
# hands each with 5-of-a-kind need some way to break the tie.
# Even worse, consider two hands of the worst value (no
# straight, no flush, no pairs). We may have to compare
# every cards in each hand to determine which is the winner.
#
# I assign each hand a "value" and a "ranking". For example:
# - the hand "AH AS AD AC AH" has the value 9 * 10^11 (5 of
#   a kind), and the ranking "14" (face value of Ace)
# - the hand "4H 4H 5C 6C JD" has the value 1 * 10^11 (one
#   pair) and the ranking "4110605" (starts with 4, and
#   other face values descending
#
# I chose such a large number for the hand value so that the
# value and the ranking can be combined into a single integer
# value. Given these hands:
# a) JH 10H 9H 2S 2C
# b) JD 10D 8D 2C 2S
#    .......^^......
#
# They both have hand value 1 * 10^11 = 100000000000
# Ranking of hand (a) is "2111009"
# Ranking of hand (a) is "2111008"
# Evaluated value of (a) is 100002111009
# Evaluated value of (b) is 100002111008
# So it's a simple numeric comparison to determine the winner.

evaluate() {
    local hand=$1

    local -a values
    readarray -t values < <(
        for card in $hand; do  #unquoted
            cardvalue "$card"
        done | sort -nr
    )

    local tiebreaker hand_value
    if tiebreaker=$(fiveOfAKind "${values[@]}"); then
        hand_value=9
    elif tiebreaker=$(straightFlush "$hand" "${values[@]}"); then
        hand_value=8
    elif tiebreaker=$(fourOfAKind "${values[@]}"); then
        hand_value=7
    elif tiebreaker=$(fullHouse "${values[@]}"); then
        hand_value=6
    elif tiebreaker=$(flush "$hand" "${values[@]}"); then
        hand_value=5
    elif tiebreaker=$(straight "${values[@]}"); then
        hand_value=4
    elif tiebreaker=$(threeOfAKind "${values[@]}"); then
        hand_value=3
    elif tiebreaker=$(twoPair "${values[@]}"); then
        hand_value=2
    elif tiebreaker=$(onePair "${values[@]}"); then
        hand_value=1
    else
        tiebreaker=$(ranking "${values[@]}")
        hand_value=0
    fi

    echo $((hand_value * (10 ** 11) + tiebreaker))
}

cardvalue() {
    local face=${1%?}
    case $face in
        A) echo 14 ;;
        K) echo 13 ;;
        Q) echo 12 ;;
        J) echo 11 ;;
        *) echo "$face" ;;
    esac
}

ranking() {
    local value
    printf -v value "%02d" "$@"
    echo $((10#$value))  # strips leading zeroes
}

fiveOfAKind()  { has_grouping "5"       "$@"; }
fourOfAKind()  { has_grouping "4 1"     "$@"; }
threeOfAKind() { has_grouping "3 1 1"   "$@"; }
fullHouse()    { has_grouping "3 2"     "$@"; }
twoPair()      { has_grouping "2 2 1"   "$@"; }
onePair()      { has_grouping "2 1 1 1" "$@"; }

flush() {
    local -a cards
    read -ra cards <<< "$1"
    shift

    local prev=${cards[0]: -1:1} suit
    for card in "${cards[@]:1}"; do
        suit=${card: -1:1}
        [[ $prev == "$suit" ]] || return $FAILURE
    done
    ranking "$@"
}

straight() {
    if [[ "$*" == "14 5 4 3 2" ]]; then
        # 5-high straight
        ranking 5 4 3 2 1
        return $SUCCESS
    else
        prev=$1
        for i in {2..5}; do
            ((prev == ${!i} + 1)) || return $FAILURE
            prev=${!i}
        done
        ranking "$@"
        return $SUCCESS
    fi
}

straightFlush() {
    local -a cards
    read -ra cards <<< "$1"
    shift
    _=$(flush "${cards[*]}" "$@") && straight "$@"
}

has_grouping() {
    local groupings=$1 g v
    local -a groups values
    shift

    local -a c
    for v; do ((c[v]++)); done

    while read -r g v; do
        groups+=("$g")
        values+=("$v")
    done < <(
        for v in "${!c[@]}"; do
            echo "${c[v]} $v"
        done | sort -k1,1nr -k2,2nr
    )

    if [[ "${groups[*]}" == "$groupings" ]]; then
        ranking "${values[@]}"
        return $SUCCESS
    fi
    return $FAILURE
}

validate() {
    local card="([234567890JQKA]|10)[HCSD]"
    local re="^$card( $card){4}$"
    [[ $1 =~ $re ]]
}

main() {
    local -A value

    for hand in "$@"; do
        validate "$hand" || die "invalid hand"
        value[$hand]=$(evaluate "$hand")
    done

    local max
    max=$(math::max "${value[@]}")

    for hand in "${!value[@]}"; do
        ((max == value[$hand])) && echo "$hand"
    done
    return $SUCCESS
}

main "$@"
