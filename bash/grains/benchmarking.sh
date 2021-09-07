#!/usr/bin/env bash

{
    echo "==================================="
    echo "Benchmarking 'total'"
    echo
    hyperfine -w 5 \
        -n 'bc total'        'bash -c ". ./grains_lib.bash; bc::total"' \
        -n 'hardcoded total' 'bash -c ". ./grains_lib.bash; precalculated::total"' \
        -n 'bash_only total' 'bash -c ". ./grains_lib.bash; bash_only::total"'

    echo
    echo "==================================="
    echo "Benchmarking 'square 63'"
    echo
    hyperfine \
        -n 'bc 63'        'bash -c ". ./grains_lib.bash; bc::atSquare 63"' \
        -n 'hardcoded 63' 'bash -c ". ./grains_lib.bash; precalculated::atSquare 63"' \
        -n 'bash_only 63' 'bash -c ". ./grains_lib.bash; bash_only::atSquare 63"'

} | tee benchmarking.out
