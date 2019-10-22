#!/usr/bin/env bash

if [[ ${BASH_VERSINFO[0]} -lt 4 ]]; then

    # the sensible straightforward solution
    echo "Hello, World!"

else

    # a goofy solution
    command_not_found_handle() { 
        local cmd=$1; shift
        local args=( "$@" )
        printf "%s %s\n" "$cmd" "${args[*]}"
    }

    Hello,      \
        World!
fi
