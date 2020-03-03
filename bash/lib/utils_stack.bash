#!/usr/bin/env bash

# look for utils.bash in the same directory as this file
source "${BASH_SOURCE[0]%/*}"/utils.bash
checkBashVersion 4.3 namerefs

# a library of useful bash functions
# works with bash version 3.2+

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

stack::len() {
    local -n __stack_len=$1
    echo ${#__stack_len[@]}
}

stack::empty() { 
    local -n __stack_empty=$1
    (( $(stack::len __stack_empty) == 0 ))
}

stack::push() { 
    local -n __stack_push=$1
    shift
    __stack_push+=("$@")
}

stack::peek() { 
    local -n __stack_peek=$1
    ! stack::empty __stack_peek && echo ${__stack_peek[-1]}
}

stack::pop() {
    local -n __stack_pop=$1
    ! stack::empty __stack_pop && unset __stack_pop[-1]
}
