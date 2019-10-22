#!/usr/bin/env bash

# a less DRY, but nicely readable solution

if (( $# == 0 )) || [[ $1 == *[^0-9]* ]]; then
    echo "usage: ${0##*/} decimalNumber"
    exit 1
fi

decimal=$1
while (( decimal > 0 )); do
    (( decimal >= 1000 )) && { printf  M; ((decimal -= 1000)); continue; }
    (( decimal >=  900 )) && { printf CM; ((decimal -=  900)); continue; }
    (( decimal >=  500 )) && { printf  D; ((decimal -=  500)); continue; }
    (( decimal >=  400 )) && { printf CD; ((decimal -=  400)); continue; }
    (( decimal >=  100 )) && { printf  C; ((decimal -=  100)); continue; }
    (( decimal >=   90 )) && { printf XC; ((decimal -=   90)); continue; }
    (( decimal >=   50 )) && { printf  L; ((decimal -=   50)); continue; }
    (( decimal >=   40 )) && { printf XL; ((decimal -=   40)); continue; }
    (( decimal >=   10 )) && { printf  X; ((decimal -=   10)); continue; }
    (( decimal >=    9 )) && { printf IX; ((decimal -=    9)); continue; }
    (( decimal >=    5 )) && { printf  V; ((decimal -=    5)); continue; }
    (( decimal >=    4 )) && { printf IV; ((decimal -=    4)); continue; }
    printf I; ((decimal--))
done
printf "\n"
