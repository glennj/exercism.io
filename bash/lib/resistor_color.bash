#!/usr/bin/env bash

# This library used for all the "resistor color" exercises

readonly COLORS=(
    black brown red orange yellow
    green blue violet grey white
)

colorValue() {
    local value
    for value in "${!COLORS[@]}"; do
        if [[ $1 == "${COLORS[value]}" ]]; then
            echo "$value"
            return
        fi
    done
    return 1
}
