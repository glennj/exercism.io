#!/bin/bash

source ./utils.bash
checkBashVersion 4.0 "associative arrays"

using_associative_array() {
    local -rA points=(
        [A]=1 [E]=1 [I]=1 [O]=1 [U]=1 [L]=1 [N]=1 [R]=1 [S]=1 [T]=1
        [D]=2 [G]=2
        [B]=3 [C]=3 [M]=3 [P]=3
        [F]=4 [H]=4 [V]=4 [W]=4 [Y]=4
        [K]=5
        [J]=8 [X]=8
        [Q]=10 [Z]=10
    )

    local sum=0
    local word=${1^^}

    for ((i = 0; i < ${#word}; i++)); do
        ((sum += ${points[${word:i:1}]:-0}))
    done
    echo "$sum"
}

using_case_glob() {

    # the integer attribute allows "bare" arithmetic evaluation
    # ref: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
    local -i sum=0

    # don't care if the input is upper or lower case
    shopt -s nocasematch

    for ((i = 0; i < ${#1}; i++)); do
        case ${1:i:1} in
            [aeioulnrst]) sum+=1 ;;
            [dg])         sum+=2 ;;
            [bcmp])       sum+=3 ;;
            [fhvwy])      sum+=4 ;;
            [k])          sum+=5 ;;
            [jx])         sum+=8 ;;
            [qz])         sum+=10 ;;
            *)            : ;; # other characters are value-free
        esac
    done
    echo $sum
}

# Mapping the letters to their score by using a sparse array
# and iterating over the indices.
# This one will be somewhat less efficient due to the nested loops
# but I would expect it to be roughly the same as the `case` solution.
using_array() {
    shopt -s nocasematch

    local groups=( [1]=AEIOULNRST [2]=DG [3]=BCMP [4]=FHVWY [5]=K [8]=JX [10]=QZ )
    local sum=0 char

    while IFS= read -d '' -n1 -r char; do
        for score in "${!groups[@]}"; do
            if [[ ${groups[score]} == *"$char"* ]]; then
                ((sum += score))
                break
            fi
        done
    done < <(printf '%s' "$1")
    echo $sum
}

#using_associative_array "$@"
#using_case_glob "$@"
using_array "$@"
