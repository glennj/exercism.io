#!/usr/bin/env bash

# requires bug fix from version 4.3
# https://git.savannah.gnu.org/cgit/bash.git/tree/CHANGES#n2144
if    [[ ${BASH_VERSINFO[0]} -lt 4 ]] ||
    { [[ ${BASH_VERSINFO[0]} -eq 4 ]] && [[ ${BASH_VERSINFO[1]} -lt 3 ]]; }
then
    echo "bash version 4.3 required" >&2
    exit 2
fi

declare -A brackets=(
    ["]"]="["
    [")"]="("
    ["}"]="{"
)
stack=""

for ((i=0; i<${#1}; i++)); do
    char=${1:i:1}
    case $char in
        "[" | "(" | "{") 
            stack+=$char
            ;;
        "]" | ")" | "}")
            if [[ -z $stack || $stack != *"${brackets[$char]}" ]]; then
                echo false
                exit
            else
                stack=${stack%?}
            fi
            ;;
    esac
done

[[ -z $stack ]] && echo true || echo false
