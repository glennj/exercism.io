#!/usr/bin/env bash

case $# in
    1) echo "Hello, $1" ;;
    *) echo >&2 "Usage: ${0##*/} <person>"; exit 1 ;;
esac
