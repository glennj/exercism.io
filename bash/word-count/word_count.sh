#!/bin/bash

declare -A words=()     # associative array
declare -a count=()     # numerically indexed array
set -f                  # disable path expansion

# first, iterate over all the whitespace-separated character sequences
for chars in $@; do     # $@ is specifically unquoted
    # replace all NON apostrophe/letter/number characters with spaces
    chars=${chars//[^\'[:alnum:]]/ }      # ' for syntax highlighing

    # now, iterate over the remaining words
    for word in $chars; do     # $chars is specifically unquoted
        # remove encosing single quotes from the word
        [[ $word == "'"*"'" ]] && word=${word:1:-1}
        word=${word,,}

        # I would like to do this directly 
        #   ((count[$word]++))
        # but bash does not like single quotes in arithmetic context:
        #   count[can't]++ : bad array subscript
        # so instead I'll have "words" as an associative array that
        # maps the word to the *index* into the "count" indexed array.

        if ! [[ -v words["$word"] ]]; then
            words[$word]=${#words[@]}
        fi
        (( count[${words[$word]}]++ ))
    done
done

for word in "${!words[@]}"; do
    echo "$word: ${count[${words[$word]}]}"
done
