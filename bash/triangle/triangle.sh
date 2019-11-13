#!/bin/bash

main() {
    case $1 in
        equilateral|scalene|isosceles) : ;;
        *) echo "unknown type $1" >&2; exit 1 ;;
    esac
    valid "${@:2}" && "$@" && echo true || echo false
}

valid() {
    local a b c
    { read a; read b; read c; } < <(printf "%s\n" "$@" | sort -g)
    (( $(echo "$a > 0 && $a + $b > $c" | bc) ))
}

equilateral() { [[ $1 == "$2" && $1 == "$3" ]]; }
isosceles()   { [[ $1 == "$2" || $1 == "$3" || $2 == "$3" ]]; }
scalene()     { ! isosceles "$@"; }

main "$@"
