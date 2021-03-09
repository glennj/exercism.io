#!/usr/bin/env bash

# globals
in_list=false
html=""

open_list() {
    if ! $in_list; then
        html+="<ul>"
        in_list=true
    fi
}

close_list() {
    if $in_list; then
        html+="</ul>"
        in_list=false
    fi
}

em()     { inline_style "$1"  _   em; }
strong() { inline_style "$1"  __  strong; }

inline_style() {
    local line=$1 delimiter=$2 tag=$3

    # Because the first `.*` is greedy, the pattern will match
    # the _last_ pair of delimiters in the line.

    while [[ $line =~ ^(.*)"$delimiter"(.+)"$delimiter"(.*) ]]; do
        printf -v line '%s<%s>%s</%s>%s' \
            "${BASH_REMATCH[1]}" \
            "$tag" \
            "${BASH_REMATCH[2]}" \
            "$tag" \
            "${BASH_REMATCH[3]}"
    done
    printf '%s' "$line"
}

main() {
    exec < "$1"

    while IFS= read -r line; do

        # Handle inline formatting.
        #
        # My method of scanning for strong and emphasized
        # text (using regular expressions) is not entirely
        # correct: I don't handle intermingled markup quite
        # right.  For example:
        #
        #     some _italic and __bold italic___ text
        #
        # is rendered as
        #
        #     some <em>italic and <strong>bold italic</em></strong> text
        #     ......^^.............^^^^^^..............^^...^^^^^^......
        #
        # instead of
        #
        #     some <em>italic and <strong>bold italic</strong></em> text
        #     ......^^.............^^^^^^..............^^^^^^...^^.......
        #
        # This is a consequence of regex greedy matching.

        line=$(strong "$line")
        line=$(em "$line")

        # Check for a list item

        if [[ $line == "*"[[:blank:]]* ]]; then
            open_list
            html+="<li>${line#??}</li>"

        else
            close_list

            if [[ $line =~ ^("#"+)[[:blank:]]+(.*) ]]; then
                tag="h${#BASH_REMATCH[1]}"
                html+="<$tag>${BASH_REMATCH[2]}</$tag>"

            else # a plain paragraph
                html+="<p>$line</p>"
            fi
        fi
    done

    close_list

    printf '%s' "$html"
}

main "$@"
