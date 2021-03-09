#!/usr/bin/env bash

n=$1
factors=()
p=2

while ((p * p <= n)); do
    if ((n % p == 0)); then
        factors+=("$p")
        ((n /= p))
    else
        ((p += (p == 2 ? 1 : 2)))
    fi
done

((n > 1)) && factors+=("$n")

echo "${factors[*]}"
