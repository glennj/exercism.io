#!/bin/bash

source ../lib/utils_string.bash

# Yelling: contains a letter and no lower-case letters

isSilent()   { [[ -z "$1" ]]; }
isQuestion() { [[ "$1" == *"?" ]]; }
isYelling()  { [[ "$1" == *[[:alpha:]]* && "$1" != *[[:lower:]]* ]]; }

input=$(str::trimright "$1")

if isSilent "$input"; then
    echo 'Fine. Be that way!'
else
    isQuestion "$input"; result+=$?
    isYelling  "$input"; result+=$?

    case $result in
        00) echo "Calm down, I know what I'm doing!" ;;
        01) echo 'Sure.' ;;
        10) echo 'Whoa, chill out!' ;;
        11) echo 'Whatever.' ;;
    esac
fi
