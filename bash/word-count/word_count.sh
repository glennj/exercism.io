#!/bin/bash

source ./utils.bash
checkBashVersion 4.0 "associative arrays, case conversion"
source ./utils_string.bash

############################################################
# Notes on the evolution on this exercise:
#
# This would be cleaner:
#   (( count[$word] += 1 ))
# but when word contains a single quote, the
# arithmetic parser breaks:
#   $ word="don't"
#   $ (( count[$word] += 1 ))
#   bash: ((: count[don't] += 1 : bad array subscript (error token is "count[don't] += 1 ")
# No amount of quoting helps.
###count[$word]=$(( ${count[$word]} + 1 ))

# Breakthrough!  This is possible, thanks to @undefined-None in
# https://exercism.io/tracks/bash/exercises/word-count/solutions/fbdc80eeb4e043ad94397050118aecb1
#
# shellcheck disable=SC2016
###(('count[$word]' += 1))

# Oh, sadness: bash 5.1 introduces some breaking behaviour:
# $ bash word_count.sh "walk don't run"
# word_count.sh: line 45: ((: 'count[walk]' += 1: syntax error: operand expected (error token is "'count[walk]' += 1")
# word_count.sh: line 45: ((: 'count[don't]' += 1: syntax error: operand expected (error token is "'count[don't]' += 1")
# word_count.sh: line 45: ((: 'count[run]' += 1: syntax error: operand expected (error token is "'count[run]' += 1")

# But, via the "bug-bash" mailing list, I found the 
# `assoc_expand_once` shopt setting:
# > If set, the shell suppresses multiple evaluation of associative
# > array subscripts during arithmetic expression evaluation, while
# > executing builtins that can perform variable assignments, and while
# > executing builtins that perform array dereferencing
############################################################

declare -A count=()
declare -l word
bashver=$(bashversion)

if ((bashver < 50)); then
    incr() {
        # array expansion is quoted
        (('count[$1]' += 1))
    }
else
    shopt -s assoc_expand_once
    incr() {
        # no quoting
        ((count[$1] += 1))
    }
fi

for sentence in "$@"; do
    # Some tests contain the literal string "\n" to mean a
    # newline: generally replace escape sequences with their
    # actual characters: use printf to interpret backslash
    # sequences. But protect any `%` characters from printf.
    #
    # shellcheck disable=SC2059
    if ((bashver < 44)); then
        printf -v sentence "${sentence//%/%%}"
    else
        # use the `${parameter@operator}` expansion
        sentence="${sentence@E}"
    fi

    # Pity bash does not do global regex matching. We have
    # to loop: find the first match, then remove it.
    while [[ $sentence =~ [[:alnum:]"'"]+ ]]; do
        word=${BASH_REMATCH[0]}

        # remove the shortest prefix ending with the word
        sentence=${sentence#*${BASH_REMATCH[0]}}

        # we've allowed single quotes as apostrophes, but
        # we don't want leading or trailing quotes.
        word=$(str::trim "$word" "'")

        # finally, increment the count for this word
        [[ -n $word ]] && incr "$word"
    done
done

for word in "${!count[@]}"; do
    echo "$word: ${count[$word]}"
done
