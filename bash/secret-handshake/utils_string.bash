#!/usr/bin/env bash

# shellcheck disable=SC2178

# A library of useful bash functions
# Works with bash version 3.2+

if ! type -t with_shopt > /dev/null; then
    # shellcheck source=/dev/null
    source "$(dirname "${BASH_SOURCE[0]}")"/utils.bash
fi

#############################################################
# String functions

# ord: the ascii value of a character
# $ ord "A" #=> 65
#
# Note the leading single quote in the last argument
# https://www.gnu.org/software/bash/manual/bash.html#index-printf
#     Arguments to non-string format specifiers (ed: such as %d)
#     are treated as C language constants, except [...] if
#     the leading character is a single or double quote, the
#     value is the ASCII value of the following character.
#
str::ord() {
    printf "%d" "\"$1"
}

# chr: the character represented by the given ASCII decimal value
# $ chr 65 #=> A
#
# Would probably be more performant to use a fixed array of
# letters and index into it, but this is pretty cool.
#
str::chr() {
    # shellcheck disable=SC2059
    printf "\x$(printf "%x" "$1")"
}

# join the elements of an array into a single string
#
# parameters
# - character to join with
# - elements to join
#
# usage: str::join "," "${fields[@]}"
#
str::join() {
    local IFS=$1
    shift
    printf '%s\n' "$*"
}

# retrieve the characters in a string
#
# parameters
# - the string to split
# - the name of an array variable to hold the chars
#
# usage:
#   str::chars "$aString" chars;  declare -p chars
#
str::chars() {
    local -n chars=$2
    [[ $1 =~ ${1//?/(.)} ]]
    chars=("${BASH_REMATCH[@]:1}")
}

# Check that the parameter is a valid integer
#
str::isInt() {
    # shellcheck disable=SC2034
    local str=$1
    # shellcheck disable=SC2016
    with_shopt extglob '[[ $str == ?([-+])+([[:digit:]]) ]]'
}

str::isFloat() {
    # shellcheck disable=SC2034
    local str=$1
    # shellcheck disable=SC2016
    with_shopt extglob '
        [[ $str == ?([-+])+([[:digit:]])?(.*([[:digit:]])) ]] ||
        [[ $str == ?([-+])*([[:digit:]]).+([[:digit:]]) ]]
    '
}

# Repeat a character a specified number of times
#
# parameters
# - character
# - count
#
# usage: strRepeat "*" 60
#
str::repeat() {
    local char=$1 count=$2
    local result
    # string of count spaces
    printf -v result "%*s" "$count" ""
    # replace spaces with the char
    printf '%s\n' "${result// /$char}"
}

# In a string variable, set the character at a specific index
# to be the given new character.
#
# parameters
# - index into the string (first char is index 0)
# - character to insert
# - variable holding the string
#
# e.g.
#     str="hello world"
#     str::putAt 4 , str
#     echo "$string"    # => 'hell, world'
#
str::putAt() {
    local -i idx=$1
    local char=${2:- }
    local -n __str=$3
    if ((0 <= idx && idx < ${#__str})); then
        __str=${__str:0:idx}${char}${__str:idx+1}
    fi
}

# is the given string a palindrome
#
str::isPalindrome() {
    local word=$1
    local -i i j len=${#word}
    for ((i = len / 2; i >= 0; i--)); do
        j=$((len - i - 1))
        [[ ${word:i:1} == "${word:j:1}" ]] || return 1
    done
}

# reverse a string
#
str::reverse() {
    local reversed=""
    for ((i = ${#1} - 1; i >= 0; i--)); do
        reversed+="${1:i:1}"
    done
    printf '%s\n' "$reversed"
}

# trim whitespace or chosen characters from the ends of a string
#
str::trimright() {
    # shellcheck disable=SC2034
    local str=$1 chars=${2:-[:space:]}
    # shellcheck disable=SC2016
    with_shopt extglob 'printf "%s\n" "${str/%+([$chars])/}"'
}

str::trimleft() {
    # shellcheck disable=SC2034
    local str=$1 chars=${2:-[:space:]}
    # shellcheck disable=SC2016
    with_shopt extglob 'printf "%s\n" "${str/#+([$chars])/}"'
}

str::trim() {
    str::trimleft "$(str::trimright "$1" "$2")" "$2"
}

# find the first index of a substring (needle) within a string (haystack)
# return -1 if haystack does not contain needle
#
str::index() {
    local needle=$1 haystack=$2
    local prefix=${haystack%%"$needle"*}
    [[ "$prefix" == "$haystack" ]] && echo -1 || printf '%s\n' "${#prefix}"
}

# add commas to a number
#
# e.g.
#    str::commify 1234567890       # => "1,234,567,890"
#    str::commify 1234567890 "."   # => "1.234.567.890"
#    str::commify 1234567890 " " 4 # => "12 3456 7890"
#
str::commify() {
    local n=$1
    local IFS=${2:-,}
    local size=${3:-3}
    local -a groups
    while [[ $n =~ (.{1,$size})$ ]]; do
        groups=("${BASH_REMATCH[1]}" "${groups[@]}")
        n=${n%"${BASH_REMATCH[1]}"}
    done
    printf '%s\n' "${groups[*]}"
}
