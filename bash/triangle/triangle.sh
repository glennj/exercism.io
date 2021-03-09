#!/usr/bin/env bash

# works with: bash 3.2.57
# external tools used:
#   bc   - arithmetic expression evaluation
#   sort - sorting numerically
#
# I'm using bc everywhere to handle arbitrary expressions
# as input:
#   $ bash triangle.sh  equilateral  0.5  1/2  '2 ^ -1'
# will output "true"

# "read without -r will mangle backslashes."
# shellcheck disable=SC2162

validate() {
    local a b c
    { read a; read b; read c; } < <(
        printf "%s\n" "$@" \
        | bc --mathlib \
        | sort --general-numeric-sort
    )
    (($(bc <<< "$a > 0 && $a + $b > $c")))
}

countEqualPairs() {
    local a b c
    { read a; read b; read c; } < <(
        printf "(%s)==(%s)\n"  "$1" "$2"  "$1" "$3"  "$2" "$3" \
        | bc --mathlib
        # this outputs 3 lines, each 1 or 0
    )
    echo $((a + b + c))
}

# counting pairs of equal sides
equilateral() { (($1 == 3)); }
isosceles()   { (($1 >= 1)); }
scalene()     { (($1 == 0)); }

main() {
    case $1 in
        equilateral | scalene | isosceles)
            local p
            p=$(countEqualPairs "${@:2:3}")

            validate "${@:2}" && "$1" "$p" &&
                echo true || echo false

            ;;
        *)  echo "unknown type: $1" >&2
            exit 1
            ;;
    esac
}

main "$@"
