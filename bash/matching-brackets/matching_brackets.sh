#!/usr/bin/env bash

source ../lib/utils.bash
checkBashVersion 4.3 "associative array fixes"

declare -A brackets=(
    ["]"]="["
    [")"]="("
    ["}"]="{"
)
stack=""

for ((i = 0; i < ${#1}; i++)); do
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

[[ -z $stack ]]
true_or_false
