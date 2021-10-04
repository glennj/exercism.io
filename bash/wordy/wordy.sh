#!/usr/bin/env bash

source ./utils.bash
source ./utils_string.bash
checkBashVersion 4.3 namerefs

subst() {
    local -n q=$1
    while [[ $q == *[[:alpha:]]* ]]; do
        prev=$q

        q=${q//plus/+}
        q=${q//minus/-}
        q=${q//multiplied by/*}
        q=${q//divided by/\/}

        if [[ $prev == "$q" ]]; then
            # we have letters, but did not match a valid op
            die "unknown operation"
        fi
    done
}

# evaluate the arithmetic operations from left to right
evaluate() {
    local -n q=$1
    while true; do
        read -r a op b rest <<< "$q"

        str::isInt "$a" || die "syntax error"

        if [[ -z $op ]]; then
            echo "$a"
            return
        fi
        [[ $op == [-+*/] ]] || die "syntax error"

        str::isInt "$b" || die "syntax error"

        # shellcheck disable=SC2086,SC1102
        q=$((a $op b))
        [[ -n $rest ]] && q+=" $rest"
    done
}

main() {
    local -l question=$1
    shopt -s extglob
    question=${question##*([[:blank:]])what is*([[:blank:]])}
    question=${question%%*([[:blank:]])?*([[:blank:]])}

    [[ -n $question ]] || die "syntax error"

    subst question && evaluate question
}

main "$@"
