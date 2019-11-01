#!/usr/bin/env bash

source ./stack.bash


# I'm going to be using unquoted variables to take advantage
# of word splitting. Disable filename expansion.
set -o noglob
shopt -s extglob


# global variables for the stack and the macros
declare -a S       # indexed array
declare -A M       # associative array


main() {
    # Read from stdin
    while IFS= read -r line; do

        # Lower-case the line, and allow word-splitting.
        # Use the positional parameters as a work array.
        set -- ${line,,}

        # Keep looping until all the positional params
        # have been consumed.
        while (( $# > 0 )); do
            word=$1
            shift

            if is_number $word; then
                stack::push S $word

            elif [[ -n ${M[$word]} ]]; then
                set -- ${M[$word]} "$@"

            else
                case $word in
                :)
                    record_macro "$@"
                    # Discard rest of line
                    set --
                    ;;
                [-+*/])
                    # Arithmetic operation
                    binary_op $word
                    ;;
                dup|drop|swap|over)
                    # Invoke this word as a command
                    $word
                    ;;
                *)
                    echo "undefined operation" >&2
                    exit 1
                    ;;
                esac
            fi
        done
    done

    echo "${S[*]}"
}


# True if the first argument consists of digits only
is_number() { [[ $1 == +([0-9]) ]]; }

die() { echo "$*" >&2; exit 1; }

record_macro() {
    local macro_name=$1
    if is_number $macro_name; then 
        die "illegal operation: cannot redefine number"
    fi
    shift

    # Check the last word
    [[ ${!#} != ";" ]] && die "macro not terminated with semicolon"
    # pop the semicolon
    set -- ${@:1:$#-1}

    (( $# == 0 )) && die "empty macro definition"

    # Check any words in definition for macros
    local definition=()
    for word; do
        if [[ -n ${M[$word]} ]]; then
            definition+=( ${M[$word]} )
        else
            definition+=( $word )
        fi
    done
    M[$macro_name]=${definition[*]}
}

# Check the size of the stack for the number of needed elements
need() {
    local -i n=$1 len
    len=$(stack::len S)
    (( n > 0 && len == 0 )) && die "empty stack"
    (( n > 1 && len == 1 )) && die "only one value on the stack"
    (( n > len ))           && die "not enough values on the stack"
}

binary_op() {
    local op=$1 a b

    need 2
    b=$(stack::peek S) && stack::pop S
    a=$(stack::peek S) && stack::pop S
    
    [[ $op == "/" ]] && (( b == 0 )) && die "divide by zero"
    stack::push S $(( a $op b ))
}

dup() {
    need 1
    stack::push S $(stack::peek S)
}

drop() {
    need 1
    stack::pop S
}

swap() {
    local a b
    need 2
    b=$(stack::peek S) && stack::pop S
    a=$(stack::peek S) && stack::pop S
    stack::push S $b $a
}

over() {
    local a b
    need 2
    b=$(stack::peek S) && stack::pop S
    a=$(stack::peek S)
    stack::push S $b $a
}

main
