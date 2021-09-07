#!/usr/bin/env bash

# I can be lax about quoting because I'm passing around integers.
# shellcheck disable=SC2086,SC2206
#
# For an examplary solution, see
# https://exercism.io/tracks/bash/exercises/bowling/solutions/7274e7c161b04100962835bae63ea518
#

source ./utils.bash
source ./utils_math.bash
checkBashVersion 4.3 namerefs

# global vars
declare -i frame=1
declare -i score=0
declare -ia current_frame
declare -ia bonuses

main() {
    local -i pins
    for arg; do
        pins=$((arg))
        ((frame > 10)) && die "Cannot roll after game is over"
        ((pins < 0))   && die "Negative roll is invalid"
        ((pins > 10))  && die "Pin count exceeds pins on the lane"
        too_many $pins && die "Pin count exceeds pins on the lane"
        add_score $pins
        handle_frame $pins
    done
    ((frame <= 10)) && die "Score cannot be taken until the end of the game"
    echo $score
}

# Check if there have been too many pins rolled for the current frame.
# This is intended to be "boolean true" if too many pins, so
# exit with status 0 if too many, status 1 if not too many
too_many() {
    local pins=$1
    if ((${#current_frame[@]} == 0)); then
        return 1
    elif ((frame == 10)); then
        too_many_tenth $pins
    else
        ((current_frame[0] + pins > 10))
    fi
}

too_many_tenth() {
    local pins=$1
    local non_strikes=()
    for roll in "${current_frame[@]}"; do
        ((roll < 10)) && non_strikes+=($roll)
    done
    local num=${#non_strikes[@]}
    if  ((num == 0)) ||
        ((num == 2 && $(math::sumArray non_strikes) == 10))
    then
        return 1        # not too many
    else
        ((non_strikes[0] + pins > 10))
    fi
}

add_score() {
    local pins=$1
    ((score += pins))
    for idx in "${!bonuses[@]}"; do
        if ((bonuses[idx] > 0)); then
            ((score += pins))
            ((bonuses[idx] -= 1))
        fi
    done
}

handle_frame() {
    if ((frame == 10)); then
        handle_tenth_frame $1
    else
        handle_nth_frame $1
    fi
}

handle_nth_frame() {
    local pins=$1

    if ((pins == 10)); then
        # It's a strike.
        # Recall that we've already checked for too many
        # pins, so if pins == 10 then it's really a strike on
        # the first ball of the frame.
        bonuses+=(2)
        ((frame += 1))
    elif ((${#current_frame[@]} == 0)); then
        # first ball of frame
        current_frame+=($pins)
    else
        # second ball of frame, is it a spare?
        if ((current_frame[0] + pins == 10)); then
            bonuses+=(1)
        fi
        ((frame += 1))
        current_frame=()
    fi
}

handle_tenth_frame() {
    local pins=$1
    current_frame+=($pins)
    local num=${#current_frame[@]}
    if  ((num == 3)) ||
        ((num == 2 && $(math::sumArray current_frame) < 10))
    then
        ((frame += 1))
    fi
}

main "$@"
