#!/usr/bin/env bash

main() {
    local results=()
    for word; do
        results+=( "$(translate "$word")" )
    done
    echo "${results[*]}"
}

translate() {
    if [[ $1 =~ ^([aeiou]|yt|xr) ]]; then   # apple, xray
        printf "%say\n" "${1}"
    elif [[ $1 =~ ^(.?qu)(.*)       ]] ||   # square, quit
         [[ $1 =~ ^([^aeiou]+)(y.*) ]] ||   # rhythm, my
         [[ $1 =~ ^([^aeiou]+)(.*)  ]]      # strength
    then
        printf "%s%say\n" "${BASH_REMATCH[2]}" "${BASH_REMATCH[1]}"
    fi
}

main "$@"
