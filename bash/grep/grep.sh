#!/usr/bin/env bash

source ../lib/utils.bash
checkBashVersion 4.0 "lowercase parameter expansion"

# Some global boolean "flags", implemented as functions

case_insensitive () { false; }
filename_only    () { false; }
line_numbers     () { false; }
full_line        () { false; }
inverted         () { false; }

main() {
    local opt pattern file
    local OPTIND OPTARG  # when using `getopts` in a function

    # Process options
    while getopts :ilnxv opt; do
        case $opt in
            i) case_insensitive () { true; } ;;
            l) filename_only    () { true; } ;;
            n) line_numbers     () { true; } ;;
            v) inverted         () { true; } ;;
            x) full_line        () { true; } ;;
            ?) echo "Unknown option: $OPTARG" >&2 ;;
        esac
    done
    shift $((OPTIND - 1))

    local pattern=$1
    shift

    case_insensitive && shopt -s nocasematch
    full_line        && pattern="^${pattern}$"

    for file; do
        process_file "$file" "$pattern" "$#"
    done
}

process_file() {
    local file=$1 pattern=$2 num_files=$3
    local -i lineno=0

    while IFS= read -r line; do
        (( lineno++ ))
        if { ! inverted &&   [[ $line =~ $pattern ]]; } ||
           {   inverted && ! [[ $line =~ $pattern ]]; }
        then
            if filename_only; then
                printf "%s\n" "$file"
                break
            fi
            output "$num_files" "$file" "$lineno" "$line"
        fi
    done < "$file"
}

output() {
    local n=$1 file=$2 lineno=$3 line=$4
    local prefix=""
    (( n > 1 ))  && prefix+="$file:"
    line_numbers && prefix+="$lineno:"
    printf "%s%s\n" "$prefix" "$line"
}

main "$@"
