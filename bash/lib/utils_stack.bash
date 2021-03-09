#!/usr/bin/env bash

# shellcheck disable=SC1090,SC2034

# look for utils.bash in the same directory as this file
if ! type -t checkBashVersion > /dev/null; then
    source "$(dirname "${BASH_SOURCE[0]}")"/utils.bash
fi
checkBashVersion 4.3 namerefs

#############################################################
# Stack functions

# one issue with namerefs is that each function in the call stack
# must use a unique variable name:
#    $ foo() { local x=42; bar x; }
#    $ bar() { baz $1; }
#    $ baz() { local -n x=$1; echo $x; }
#    $ foo
#    bash: local: warning: x: circular name reference
#    bash: warning: x: circular name reference
#    bash: warning: x: circular name reference

# Note that `push` and `pop` **cannot** be done in a command
# substitution, because the changes to the stack variable will
# not persist from the subshell to the parent shell:
#     stack=()
#     stack::push stack a b c
#     ( stack::push stack d e f )
#     declare -p stack  # => declare -a stack=([0]="a" [1]="b" [2]="c")
# This is clearly inconvenient, but you have to do this:
#     val=$(stack::peek stack) && stack::pop stack

# See also the "deque" functions in utils_array.bash

stack::len() {
    local -n __stack_len=$1
    echo ${#__stack_len[@]}
}

stack::empty() {
    local -n __stack_empty=$1
    (($(stack::len __stack_empty) == 0))
}

stack::push() {
    local -n __stack_push=$1
    shift
    __stack_push+=("$@")
}

# Return the last element.
# If passed 2 arguments, populate the 2nd nameref with the
# value of the last element.
stack::peek() {
    local -n __stack_peek=$1
    if ! stack::empty __stack_peek; then
        if (($# == 1)); then
            echo "${__stack_peek[-1]}"
        else
            local -n __stack_peek_elem=$2
            __stack_peek_elem=${__stack_peek[-1]}
        fi
    fi
}

# Remove the last element.
# If passed 2 arguments, populate the 2nd nameref with the
# value of the last element.
stack::pop() {
    local -n __stack_pop=$1
    if [[ $2 ]]; then
        local -n __stack_pop_elem=$2
        __stack_pop_elem=$(stack::peek __stack_pop)
    fi
    ! stack::empty __stack_pop && unset '__stack_pop[-1]'
}
