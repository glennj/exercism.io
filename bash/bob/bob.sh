#!/bin/bash

source ../lib/utils_string.bash

isQuestion() { [[ "$1" == *"?" ]]; boolean $?; }
isShouting() { 
    # contains a letter and contains no lower-case letters
    [[ "$1" == *[[:alpha:]]* && "$1" != *[[:lower:]]* ]]
    boolean $?
}
isSilent()   { [[ -z "$1" ]]; boolean $?; }
boolean()    { (( "$1" == 0 )) && echo true || echo false; }

input=$(str::trimright "$1")
q=$(isQuestion "$input")
s=$(isShouting "$input")
z=$(isSilent   "$input")

if   $q && $s; then echo "Calm down, I know what I'm doing!"
elif $q;       then echo 'Sure.'
elif $s;       then echo 'Whoa, chill out!'
elif $z;       then echo 'Fine. Be that way!'
else                echo 'Whatever.'
fi
