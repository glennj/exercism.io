#!/bin/bash

source ../lib/utils.bash
checkBashVersion 4.0 "associative arrays"

declare -A count=()

for sentence in "$@"; do
    # Some tests contain the literal string "\n" to mean a 
    # newline: generally replace escape sequences with their
    # actual characters:
    printf -v sentence "${sentence//%/%%}"

    # Pity bash does not do global regex matching. We have
    # to loop: find the first match, then remove it.
    while [[ $sentence =~ [[:alnum:]"'"]+ ]]; do

        word=${BASH_REMATCH[0]}

        # remove the shortest prefix ending with $word
        sentence=${sentence#*$word}

        lc=${word,,}

        # we've allowed single quotes as apostrophes, but
        # we don't want leading or trailing quotes.
        lc=${lc#\'}
        lc=${lc%\'}

        # This would be cleaner:
        #   (( count[$lc] += 1 ))
        # but when lc contains a single quote, the 
        # arithmetic parser breaks:
        #   $ lc="don't"
        #   $ (( count[$lc] += 1 ))
        #   bash: ((: count[don't] += 1 : bad array subscript (error token is "count[don't] += 1 ")
        # No amount of quoting helps.
        count[$lc]=$(( ${count[$lc]} + 1 ))
    done
done

for word in "${!count[@]}"; do
    echo "$word: ${count[$word]}"
done
