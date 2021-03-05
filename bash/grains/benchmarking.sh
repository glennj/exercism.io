#!/usr/bin/env bash

{
    echo "==================================="
    echo "Benchmarking 'total'"
    echo
    hyperfine \
        -n 'bc total'        'bash grains_bc.sh total' \
        -n 'hardcoded total' 'bash grains_precalculated.sh total' \
        -n 'bash_only total' 'bash grains_bash_only.sh total'

    echo
    echo "==================================="
    echo "Benchmarking 'square 63'"
    echo
    hyperfine \
        -n 'bc 63'        'bash grains_bc.sh 63' \
        -n 'hardcoded 63' 'bash grains_precalculated.sh 63' \
        -n 'bash_only 63' 'bash grains_bash_only.sh 63'

    echo
    echo "==================================="
    echo "Benchmarking 'square 2'"
    echo
    hyperfine \
        -n 'bc 2'        'bash grains_bc.sh 2' \
        -n 'hardcoded 2' 'bash grains_precalculated.sh 2' \
        -n 'bash_only 2' 'bash grains_bash_only.sh 2'

    echo
    echo "==================================="
    echo "Benchmarking error case"
    echo
    hyperfine  --ignore-failure \
        -n 'bc 65'        'bash grains_bc.sh 65' \
        -n 'hardcoded 65' 'bash grains_precalculated.sh 65' \
        -n 'bash_only 65' 'bash grains_bash_only.sh 65'

} | tee benchmarking.out
