#!/usr/bin/env bash

source ./utils.bash
checkBashVersion 4.0 "uppercase parameter expansion"

shopt -s extglob


# first approach, split the input into words, using
# a custom set of "non-word" characters (IFS): take the
# first alpha character of each word.
word_splitting() {
    local acronym=""
    local IFS=" -"      # space and hyphen separate words
    local -a words
    local word

    read -ra words <<<"$1"    # split the input into words
    for word in "${words[@]}"; do
        # remove leading non-alpha characters
        word=${word/#+([^[:alpha:]])/}
        acronym+=${word:0:1}
    done

    # without using an array:
    ##set -f                    # disable path expansion
    ##for word in $1; do        # $1 specifically unquoted
    ##    word=${word/#+([^[:alpha:]])/}
    ##    acronym+=${word:0:1}
    ##done

    echo "${acronym^^}"
}


# second approach, iterate over the characters of the input
# and use a state machine to determine when to add a letter
# to the acronym
state_machine() {
    local state="A" char acronym=""

    # loop over the input one character at a time
    while IFS= read -r -n1 char; do

        case $state in
            # state "A": looking for the next alpha
            # character to add to the acronym
            A)  if [[ $char == [[:alpha:]] ]]; then
                    acronym+=${char^}
                    state="N"
                fi
                ;;

            # state "N": looking for the next non-alpha
            # character: when found we'll start looking for
            # the next alpha. Apostrophes are special.
            N)  if ! [[ $char == @([[:alpha:]]|"'") ]]; then
                    state="A"
                fi
                ;;
        esac

    done <<< "$1"

    echo "$acronym"
}


# third approach, use regular expression matching to find
# letters that are not preceded by a letter. Since bash does
# not implement global matching, this needs to be in a loop.
# The leading (greedy) `.*` means this matches the 
# _last_ word in the phrase.
regex_matching() {

    # Adding a leading non-letter simplifies the regex a bit.
    # Remove apostrphoes from the input.
    local phrase=">${1//"'"/}"
    local result

    while [[ $phrase =~ (.*)[^[:alpha:]]([[:alpha:]]) ]]; do
        result=${BASH_REMATCH[2]}$result
        phrase=${BASH_REMATCH[1]}
    done

    echo "${result^^}"
}


#word_splitting "$1"
#state_machine "$1"
regex_matching "$1"
