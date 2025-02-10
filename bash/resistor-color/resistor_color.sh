#!/usr/bin/env bash

source "./resistor_color.bash"

case "$1" in
    code) colorValue "$2" ;;
    colors) printf '%s\n' "${COLORS[@]}" ;;
esac
