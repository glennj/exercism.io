#!/usr/bin/env bash

# debug
# set -x

declare -i INPUT=$1

grains() {
  if (( INPUT < 1 || INPUT > 64)); then
    echo 'Error: invalid input'
    exit 1
  else
    declare -i SUM=$((2**(INPUT-1)))
    echo "${SUM#-}"
  fi
}

grains
