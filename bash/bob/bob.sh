#!/bin/bash

if [[ ${BASH_VERSINFO[0]} -lt 4 ]]; then
    echo "bash version 4.0 required" >&2
    exit 2
fi

shopt -s extglob

isQuestion() { [[ $1 == *"?" ]] && echo true || echo false; }
isShouting() { [[ $1 == *[[:alpha:]]* && $1 == "${1^^}" ]] && echo true || echo false; }
trimright() { echo "${1/%+([[:space:]])}"; }

input=$(trimright "$1")

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
