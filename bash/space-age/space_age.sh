#!/usr/bin/env bash

secondsPerEarthYear=31557600

declare -A relativeOrbit=(
    [Mercury]=0.2408467
    [Venus]=0.61519726
    [Earth]=1.0
    [Mars]=1.8808158
    [Jupiter]=11.862615
    [Saturn]=29.447498
    [Uranus]=84.016846
    [Neptune]=164.79132
)

if [[ -z ${relativeOrbit[$1]} ]]; then
    echo "error: not a planet: $1" >&2
    exit 1
fi

# bash can't actually do floating point math.
# calling out to awk to do it
awk -v age="$2" \
    -v year="$secondsPerEarthYear" \
    -v planet="${relativeOrbit[$1]}" \
    'BEGIN {printf "%.2f", age / year / planet}'