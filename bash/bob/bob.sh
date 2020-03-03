#!/bin/bash

source ../lib/utils.bash
source ../lib/utils_string.bash
checkBashVersion 4.0 "uppercase parameter expansion"

isQuestion() { [[ $1 == *"?" ]] && echo true || echo false; }
isShouting() { [[ $1 == *[[:alpha:]]* && $1 == "${1^^}" ]] && echo true || echo false; }

input=$(str::trimright "$1")

if [[ -z "$input" ]]; then
    echo 'Fine. Be that way!'
else
    q=$(isQuestion "$input")
    s=$(isShouting "$input")
    if   $q && $s; then echo "Calm down, I know what I'm doing!"
    elif $q;       then echo 'Sure.'
    elif $s;       then echo 'Whoa, chill out!'
    else                echo 'Whatever.'
    fi
fi
