#!/usr/bin/env bash

if (( $# == 0 )) || [[ $1 == *[^0-9]* ]]; then
    echo "usage: ${0##*/} decimalNumber"
    exit 1
fi

decimal=$1
roman=(
    [1000]=M  [900]=CM  [500]=D  [400]=CD
     [100]=C   [90]=XC   [50]=L   [40]=XL
      [10]=X    [9]=IX    [5]=V    [4]=IV
       [1]=I
)

for value in 1000 900 500 400 100 90 50 40 10 9 5 4 1; do
    while (( decimal >= value )); do
        printf "%s" "${roman[value]}"
        (( decimal -= value ))
    done
    (( decimal == 0 )) && break
done
printf "\n"
