#!/usr/bin/env bash

source ./utils.bash
source ./resistor_color.bash

result=0
for color in "${@:1:2}"; do
    value=$(colorValue "$color") || die "invalid color"
    ((result = 10 * result + value))
done
echo "$result"
 