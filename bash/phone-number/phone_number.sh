#!/usr/bin/env bash

source ./utils.bash

readonly err='Invalid number.  [1]NXX-NXX-XXXX N=2-9, X=0-9'

phone=${1//[^0-9]/}

# remove leading 1 if length is 11
((${#phone} == 11)) && phone=${phone#1}

# validate length
assert "${#phone} == 10" "$err"

# validate first and fourth digit
for i in 0 3; do
    digit=${phone:i:1}
    refute -C [ "$digit" = 0 -o "$digit" = 1 ] "$err"
done

echo "$phone"
