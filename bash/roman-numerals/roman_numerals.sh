#!/usr/bin/env bash

if (($# == 0)) || [[ $1 == *[^0-9]* ]] || (($1 <= 0)); then
    echo "usage: ${0##*/} positiveInteger"
    exit 1
fi

readonly roman=(
    [1000]=M  [900]=CM  [500]=D  [400]=CD
     [100]=C   [90]=XC   [50]=L   [40]=XL
      [10]=X    [9]=IX    [5]=V    [4]=IV
       [1]=I
)
# so we don't need to hardcode the decimal values twice
readonly values=(${!roman[@]})

decimal=$1

for ((i = ${#values[@]} - 1; i >= 0; i--)); do
    value=${values[i]}
    while ((decimal >= value)); do
        printf "%s" "${roman[value]}"
        ((decimal -= value))
    done
    ((decimal == 0)) && break
done
printf "\n"
