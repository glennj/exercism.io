#!/bin/bash

source ../lib/utils_string.bash

isQuestion() { [[ "$1" == *"?" ]]; trueOrFalse; }
isYelling () { 
    # contains a letter and contains no lower-case letters
    [[ "$1" == *[[:alpha:]]* && "$1" != *[[:lower:]]* ]]
    trueOrFalse
}
isSilent()    { [[ -z "$1" ]]; trueOrFalse; }
trueOrFalse() { (( $? == 0 )) && echo true || echo false; }

main() {
    input=$(str::trimright "$1")
    q=$(isQuestion "$input")
    y=$(isYelling  "$input")
    z=$(isSilent   "$input")

    if   $q && $y; then echo "Calm down, I know what I'm doing!"
    elif $q;       then echo 'Sure.'
    elif $y;       then echo 'Whoa, chill out!'
    elif $z;       then echo 'Fine. Be that way!'
                   else echo 'Whatever.'
    fi
}

main "$@"
