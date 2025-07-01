#!/usr/bin/env bash

# global vars
row_count=$#
row_width=${#1}
field=( "$@" )

isFlower() {
    [[ "${field[$1]:$2:1}" == "*" ]] && echo 1 || echo 0
}

count_neighbours() {
    local x=$1 y=$2 count=0
    if (( $(isFlower "$x" "$y") )); then 
        count=9
    else
        local dy dy
        for dx in -1 0 1; do
            (( 0 <= x + dx && x + dx < row_count)) || continue
            for dy in -1 0 1; do
                (( 0 <= y + dy && y + dy < row_width )) || continue
                (( dx == 0 && dy == 0 )) && continue
                (( count += $(isFlower "$((x + dx))" "$((y + dy))") ))  # "highlightjs
            done
        done
    fi
    echo "$count"
}

main() {
    local x y n row
    for ((x = 0; x < row_count; x++)); do
        row=""
        for ((y = 0; y < row_width; y++)); do
            n=$(count_neighbours "$x" "$y")
            case $n in
                0) row+=" " ;;
                9) row+="*" ;;
                *) row+="$n" ;;
            esac
        done
        echo "$row"
    done
}

main
