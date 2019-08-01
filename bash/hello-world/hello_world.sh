#!/usr/bin/env bash

command_not_found_handle() { 
    local cmd=$1; shift
    local args=("${@}")
    printf "%s %s\n" "$cmd" "${args[*]}"
}

        Hello,      \
        World!
