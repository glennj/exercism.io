#!/usr/bin/env bash

source ../lib/utils.bash
checkBashVersion 4.3 "stacks (namerefs)"
source ../lib/utils_string.bash
source ../lib/utils_stack.bash

# I'm going to be using unquoted variables to take advantage
# of word splitting. Disable filename expansion.
set -o noglob

# global variables for the stack and the macros
declare -a S       # indexed array
declare -A M       # associative array

############################################################
evaluate() {
    # Lower-case the line, and allow word-splitting.
    # Use the positional parameters as a work array.
    # shellcheck disable=SC2086
    set -- ${line,,}

    # Keep looping until all the positional params
    # have been consumed.
    while (($# > 0)); do
        word=$1
        shift

        if str::isInt "$word"; then
            stack::push S "$word"

        elif [[ -n ${M[$word]} ]]; then
            # it's a macro: expand it
            read -ra macroWords <<< "${M[$word]}"
            set -- "${macroWords[@]}" "$@"

        else
            case $word in
                :)
                    record_macro "$@"
                    # Discard rest of line
                    set --
                    ;;
                [-+*/])
                    # Arithmetic operation
                    arithmetic "$word"
                    ;;
                dup | drop | swap | over)
                    # Invoke this word as a command
                    $word
                    ;;
                *)
                    die "undefined operation"
                    ;;
            esac
        fi
    done
}

############################################################
record_macro() {
    local macro_name=$1
    refute -C str::isInt "$macro_name" "illegal operation: cannot redefine number"
    shift

    # Check the last word
    assert -C [ "${!#}" = ";" ] "macro not terminated with semicolon"
    # pop the semicolon
    set -- "${@:1:$#-1}"

    assert "$# > 0" "empty macro definition"

    # Check any words in definition for macros
    local definition=()
    for word; do
        definition+=("${M[$word]:-$word}")
    done
    M[$macro_name]=${definition[*]}
}

############################################################
# Check the size of the stack for the number of needed elements
need() {
    local -i n=$1 len
    len=$(stack::len S)
    refute "n > 0 && len == 0" "empty stack"
    refute "n > 1 && len == 1" "only one value on the stack"
    refute "n > len"           "not enough values on the stack"
}

arithmetic() {
    local op=$1 a b

    need 2
    stack::pop S b
    stack::pop S a

    [[ $op == "/" ]] && ((b == 0)) && die "divide by zero"

    # shfmt cannot handle the dynamic expression
    # shellcheck disable=SC1102,SC2046,SC2086
    stack::push S $(( a $op b ))
}

dup() {
    need 1
    # shellcheck disable=SC2046
    stack::push S $(stack::peek S)
}

drop() {
    need 1
    stack::pop S
}

swap() {
    local a b
    need 2
    stack::pop S b
    stack::pop S a
    stack::push S "$b" "$a"
}

over() {
    local a b
    need 2
    stack::pop S b
    stack::peek S a
    stack::push S "$b" "$a"
}

############################################################
main() {
    # consume stdin
    while IFS= read -r line; do
        evaluate "$line"
    done
    echo "${S[*]}"
}

main
