#!/bin/bash

declare -A count=()
declare -l word         # value is lower-cased

for sentence in "$@"; do
    # Some tests contain the literal string "\n" to mean a 
    # newline: replace those characters with a actual newline.
    sentence="${sentence//\\n/$'\n'}"

    # Pity bash does not do global regex matching. We have
    # to loop: find the first match, then remove it.
    while [[ $sentence =~ [[:alnum:]"'"]+ ]]; do

        word=${BASH_REMATCH[0]}

        # we've allowed single quotes as apostrophes, but
        # we don't want leading or trailing quotes.
        [[ $word == "'"* ]] && word=${word#?}
        [[ $word == *"'" ]] && word=${word%?}

        # This would be cleaner:
        #   (( count[$word] += 1 ))
        # but when word contains a single quote, the 
        # arithmetic parser breaks:
        #   $ word="don't"
        #   $ (( count[$word] += 1 ))
        #   bash: ((: count[don't] += 1 : bad array subscript (error token is "count[don't] += 1 ")
        # No amount of quoting helps.
        count[$word]=$(( ${count[$word]} + 1 ))

        # remove the prefix ending with $word
        # but don't use $word because it has been lowercased
        sentence=${sentence#*${BASH_REMATCH[0]}}
    done
done

for word in "${!count[@]}"; do
    echo "$word: ${count[$word]}"
done
