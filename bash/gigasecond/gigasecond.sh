#!/usr/bin/env bash

source ./utils.bash
source ./utils_date.bash

readonly gigasecond=1000000000
readonly format="%Y-%m-%dT%H:%M:%S"

assert "$# == 1" "usage: ${0##*/} time-spec"
timestamp=$1

# BSD date requires the timestamp to exactly match the format for parsing
[[ $timestamp == ????-??-?? ]] && timestamp+="T00:00:00"

if epoch=$(date::parse -f "$format" -u "$timestamp"); then
    date::format -f "$format" -u $((epoch + gigasecond))
else
    die "Invalid time-spec '$1'"
fi
