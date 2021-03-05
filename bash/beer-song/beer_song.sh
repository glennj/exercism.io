#!/bin/bash

source ../lib/utils.bash
checkBashVersion 4.0 "uppercase parameter expansion"

verse() {
    local -i n=$(($1))
    local w="on the wall" b
    b=$(bottles $n)
    printf "%s %s, %s.\n" "${b^}" "$w" "$b"
    b=$(bottles $((n - 1)))
    printf "%s, %s %s.\n" "$(action $n)" "$b" "$w"
}

bottles() {
    local n=$1 num s
    ((n < 0)) && n=99
    ((n == 0)) && num="no more" || num=$n
    ((n == 1)) && s="" || s="s"
    printf "%s bottle%s of beer" "$num" "$s"
}

action() {
    local n=$1
    if ((n == 0)); then
        echo "Go to the store and buy some more"
    else
        local one
        ((n == 1)) && one="it" || one="one"
        echo "Take $one down and pass it around"
    fi
}

main() {
    case $# in
        1)  verse "$1" ;;
        2)  if (($1 <= $2)); then
                echo "Start must be greater than End" >&2
                exit 1
            fi
            for ((i = $1; i >= $2; i--)); do
                verse "$i"
                echo
            done
            ;;
        *)  echo "1 or 2 arguments expected" >&2
            echo "usage: ${0##*/} verse_num" >&2
            echo "   or: ${0##*/} start end" >&2
            exit 2
            ;;
    esac
}

main "$@"
