#!/bin/bash

main() {
    case $1 in
        equilateral|scalene|isosceles) : ;;
        *) echo "unknown type $1" >&2; exit 1 ;;
    esac
    valid "${@:2}" && "$@" && echo true || echo false
}

valid() {
    readarray -t sorted < <(printf "%s\n" "$@" | sort -g)
    [[ "$(printf "%s + %s > %s\n" "${sorted[@]}" | bc)" == "1" ]]
}

equilateral() { [[ $1 == "$2" && $1 == "$3" ]]; }
isosceles()   { [[ $1 == "$2" || $1 == "$3" || $2 == "$3" ]]; }
scalene()     { ! isosceles "$@"; }

main "$@"
