#!/usr/bin/env bash

# works with bash v3.2.57

declare -ra regex=(
    '^([aeiou]|yt|xr)'   # apple, xray
    '^(.?qu)(.*)'        # square, quit
    '^([^aeiou]+)(y.*)'  # rhythm, my
    '^([^aeiou]+)(.*)'   # square
)

translate() {
    if [[ $1 =~ ${regex[0]} ]]; then
        printf "%say\n" "$1"

    elif [[ $1 =~ ${regex[1]} ]] ||
         [[ $1 =~ ${regex[2]} ]] ||
         [[ $1 =~ ${regex[3]} ]]
    then
        printf "%s%say\n" "${BASH_REMATCH[2]}" "${BASH_REMATCH[1]}"
    fi
}

main() {
    local results=()
    for word; do
        results+=("$(translate "$word")")
    done
    echo "${results[*]}"
}

main "$@"
