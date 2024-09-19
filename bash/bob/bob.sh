#!/bin/bash

# solving with extended glob patterns
shopt -s extglob

input=${1//[[:space:]]/}

# yelling is "contains an uppercase but no lowercase"
# question ends with a question mark.

case $input in
    *([^[:lower:]])[[:upper:]]*([^[:lower:]])[?] )
        echo "Calm down, I know what I'm doing!" ;;
    *([^[:lower:]])[[:upper:]]*([^[:lower:]]) )
        echo 'Whoa, chill out!' ;;
    *[?] )
        echo 'Sure.' ;;
    '' )
        echo 'Fine. Be that way!' ;;
    * )
        echo 'Whatever.' ;;
esac
