#!/usr/bin/env bash

source ../lib/utils_string.bash

reverse () {
    # cheating
    #rev <<< "$1"

    # implement in bash
    str::reverse "$1"
}

reverse "$*"
