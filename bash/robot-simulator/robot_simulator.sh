#!/usr/bin/env bash

source ./utils.bash
checkBashVersion 4.3 "-v operator"

readonly -A turn=(
    [north,R]=east [north,L]=west
    [east,R]=south [east,L]=north
    [south,R]=west [south,L]=east
    [west,R]=north [west,L]=south
)
readonly -A dx=([north]=0 [east]=1 [south]=0 [west]=-1)
readonly -A dy=([north]=1 [east]=0 [south]=-1 [west]=0)

declare -i x=${1:-0}
declare -i y=${2:-0}
declare direction=${3:-north}
declare instructions=$4

assert -C [ -v "dx[$direction]" ] "invalid direction: $direction"

for ((i = 0; i < ${#instructions}; i++)); do
    inst=${instructions:i:1}
    case $inst in
        [LR])
            direction=${turn[$direction,$inst]}
            ;;
        A)  ((x += dx[$direction]))
            ((y += dy[$direction]))
            ;;
        *)  die "invalid instruction: $inst" ;;
    esac
done

# shellcheck disable=SC2086
printf "%d %d %s\n" $x $y $direction
