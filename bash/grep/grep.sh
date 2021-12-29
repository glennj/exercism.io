#!/usr/bin/env bash

# Works with bash v3.2.57 and up.

# shellcheck disable=SC2094

# Some global boolean "flags", implemented as functions
filename_only() { false; }
line_numbers()  { false; }
inverted()      { false; }

# Print a matched line, prefixed by filename and line number,
# depending on options selected
output() {
    local n=$1 file=$2 lineno=$3 line=$4

    local items=()
    ((n > 1))    && items+=("$file")
    line_numbers && items+=("$lineno")
    items+=("$line")

    local IFS=":"
    printf "%s\n" "${items[*]}"
}

# Determine if a line matches.
# Relies on "nocaseglob" options for case insensitivity.
matches() {
    local line=$1 pattern=$2
    { ! inverted &&   [[ $line =~ $pattern ]]; } ||
    {   inverted && ! [[ $line =~ $pattern ]]; }
}

# Read the lines of a file and find matches.
process_file() {
    local file=$1 pattern=$2 num_files=$3
    local -i lineno=0

    while IFS= read -r line; do
        ((lineno++))
        if matches "$line" "$pattern"; then
            if filename_only; then
                printf "%s\n" "$file"
                break
            fi
            output "$num_files" "$file" "$lineno" "$line"
        fi
    done < "$file"
}

main() {
    local opt pattern file
    local OPTIND OPTARG  # when using `getopts` in a function
    local pattern_fmt='%s'

    # Process options
    while getopts :ilnxv opt; do
        case $opt in
            l) filename_only() { true; } ;;
            n) line_numbers()  { true; } ;;
            v) inverted()      { true; } ;;
            i) shopt -s nocasematch ;;
            x) pattern_fmt='^%s$' ;;
            ?) echo "Unknown option: $OPTARG" >&2 ;;
        esac
    done
    shift $((OPTIND - 1))

    local pattern
    # shellcheck disable=SC2059
    printf -v pattern "$pattern_fmt" "$1"
    shift

    for file; do
        process_file "$file" "$pattern" "$#"
    done
}

main "$@"
