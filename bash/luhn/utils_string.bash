#!bash

shopt -s extglob

# a library of useful bash functions
# works with bash version 3.2+

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
    printf "%d" "'$1"
}


# chr: the character represented by the given ASCII decimal value
# $ chr 65 #=> A
#
# Would probably be more performant to use a fixed array of
# letters and index into it, but this is pretty cool.
#
str::chr() {
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
    echo "$*"
}


# Check that the parameter is a valid integer
#
str::isInt() { [[ $1 == ?([-+])+([[:digit:]]) ]]; }

str::isFloat() { 
    [[ $1 == ?([-+])+([[:digit:]])?(.*([[:digit:]])) ]] ||
    [[ $1 == ?([-+])*([[:digit:]]).+([[:digit:]]) ]]
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
    echo "${result// /$char}"
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
    if (( 0 <= idx && idx < ${#__str} )); then
        __str=${__str:0:idx}${char}${__str:idx+1}
    fi
}


# is the given string a palindrome
#
str::isPalindrome() {
    local word=$1
    local -i i j len=${#word}
    for ((i = len / 2; i >= 0; i--)); do
        j=$(( len - i - 1 ))
        [[ ${word:i:1} == "${word:j:1}" ]] || return 1
    done
}


# reverse a string
#
str::reverse () {
    local reversed=""
    for (( i=${#1}-1; i >= 0; i-- )); do
        reversed+="${1:i:1}"
    done
    echo "$reversed"
}


# trim whitespace from the ends of a string
#
str::trimright() { echo "${1/%+([[:space:]])}"; }
str::trimleft()  { echo "${1/#+([[:space:]])}"; }
str::trim()      { str::trimleft "$(str::trimright "$1")"; }

