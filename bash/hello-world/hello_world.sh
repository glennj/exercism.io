#!/usr/bin/env bash

if [[ ${BASH_VERSINFO[0]} -lt 4 ]]; then

    # the sensible straightforward solution
    echo "Hello, World!"

else

    # a goofy solution
    command_not_found_handle() {
        local -l cmd=$1; shift
        local -l args=( "$@" )
        printf '%s, %s!\n' "${cmd^}" "${args[*]^}"
    }

    hello WORLD

fi
