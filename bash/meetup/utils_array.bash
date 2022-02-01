#!/usr/bin/env bash

# shellcheck disable=SC1090

# look for utils.bash in the same directory as this file
if ! type -t checkBashVersion > /dev/null; then
    source "${BASH_SOURCE[0]%/*}"/utils.bash
fi
checkBashVersion 4.3 namerefs

if ! type -t str::join > /dev/null; then
    source "${BASH_SOURCE[0]%/*}"/utils_string.bash
fi

#
# reverse the contents of an array in-place
#
# e.g.
#
#   ary=(foo bar baz qux)
#   declare -p ary
#   # => declare -a ary=([0]="foo" [1]="bar" [2]="baz" [3]="qux")
#
#   array::reverse ary
#   declare -p ary
#   # => declare -a ary=([0]="qux" [1]="baz" [2]="bar" [3]="foo")
#
array::reverse() {
    local -n __array_reverse=$1
    local -i a b
    local tmp

    for ((a = 0, b = ${#__array_reverse[@]} - 1; a < b; a++, b--)); do
        tmp=${__array_reverse[a]}
        __array_reverse[a]=${__array_reverse[b]}
        __array_reverse[b]=$tmp
    done
}

#
# create an array with "repeat" copies of the given string
#
# e.g.
#
#   array::repeat ary "hello" 10
#   declare -p ary
#
#   # => declare -a ary=([0]="hello" [1]="hello" [2]="hello"
#                        [3]="hello" [4]="hello" [5]="hello"
#                        [6]="hello" [7]="hello" [8]="hello"
#                        [9]="hello")
#
array::repeat() {
    local -n __array_repeat=$1
    local value=$2 n=$3 i
    __array_repeat=()
    for ((i = 0; i < n; i++)); do
        __array_repeat+=("$value")
    done
}

# split a string into an array using one of the given characters
# as the field separator
#
# e.g.
#   array::split ary ",-" "foo,bar-baz"
#   declare -p ary
#
#   # => declare -a ary=([0]="foo" [1]="bar" [2]="baz")
#
# See `str::join` in utils_string.bash for the reverse operation
#
array::split() {
    local -n __array_split=$1
    local IFS=$2 string=$3
    # shellcheck disable=SC2034
    read -ra __array_split <<< "$string"
}

# for convenience
#
#   array::split ary ",-" "foo,bar-baz"
#   array::join  ary ":"
#
#   # => foo:bar:baz
#
array::join() {
    local -n __array_join=$1
    local IFS=$2
    str::join "$IFS" "${__array_join[@]}"
}

# deque functions:

array::push() {
    local -n __array_push=$1
    __array_push+=("$2")
}

array::unshift() {
    local -n __array_unshift=$1
    __array_unshift=("$2" "${__array_unshift[@]}")
}

# The `pop` and `shift` functions do NOT emit the value, just remove it.
# This is because they don't work in subshells. Suppose we have
#       array::pop() {
#           local -n __array_pop=$1
#           echo "${___array_pop[-1]}"
#           unset "__array_pop[-1]"
#       }
# Then naturally we would do this:
#       value=$( array::pop myArray )
# But the function executes in a subshell, which interferes with
# the name referencing, and the array in the current shell is
# untouched:
#       $ myArray=(a b c d e f)
#       $ declare -p myArray
#       declare -a myArray=([0]="a" [1]="b" [2]="c" [3]="d" [4]="e" [5]="f")
#       $ value=$( array::pop myArray )
#       $ declare -p value myArray
#       declare -- value="f"
#       declare -a myArray=([0]="a" [1]="b" [2]="c" [3]="d" [4]="e" [5]="f")
# It is required to do this instead:
#       value=${myArray[-1]}
#       array::pop myArray
# Inconvenient, but it is what it is.

array::pop() {
    # shellcheck disable=SC2034
    local -n __array_pop=$1
    unset "__array_pop[-1]"
}

# the simple `unset 'ary[0]'` does not shift the other elements down
array::shift() {
    local -n __array_shift=$1
    __array_shift=("${__array_shift[@]:1}")
}

# find the index of the given string, or -1 if not found
#
# e.g.
#   ary=(foo bar baz)
#   array::index ary "baz"   # => 2
#   array::index ary "bazz"  # => -1
#
array::index() {
    local -n __array_index=$1
    local string=$2
    local len=${#__array_index[@]} idx=-1 i
    for ((i = 0; i < len; i++)); do
        if [[ ${__array_index[i]} == "$string" ]]; then
            idx=$i
            break
        fi
    done
    echo "$idx"
}
