#!/usr/bin/env gawk -f

@include "die"
@include "join"

# Change making algorithm from
# http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf

BEGIN {
    MAX_INT = PROCINFO["prec_max"]
    # awk does not have a "nil" data type. We need a sentinel value
    SENTINEL = SUBSEP
}

NR == 1 { for (i = 1; i <= NF; i++) denominations[i] = $i }
NR == 2 { target = $0 }

END {
    if (target == 0) exit
    assert(target > 0, "target can't be negative")

    Change(denominations, target, firstCoin)

    assert(firstCoin[target] != SENTINEL, "can't make target with given coins")

    MakeChange(firstCoin, denominations, target, coins)
    print join(coins, 1, length(coins))
}

function Change(d, n, S,             C, k, p, min, i, coin) {
    #   This function generates two arrays:
    #
    #   C = maps the minimum number of coins required to make change
    #       for each n from 1 to amount.  
    #
    #   S = the _first_ coin used to make change for amount n
    #       (actually stores the coin _index_ into the coins array)

    k = length(d)
    C[0] = 0
    for (p = 1; p <= n; p++) {
        min = MAX_INT
        coin = SENTINEL
        for (i = 1; i <= k; i++) {
            if (d[i] <= p) {
                if (1 + C[p - d[i]] < min) {
                    min = 1 + C[p - d[i]]
                    coin = i
                }
            }
        }
        C[p] = min
        S[p] = coin
    }
}

function MakeChange(S, d, n, coins,     coin, c) {
    while (n > 0) {
        coin = d[S[n]]
        coins[++c] = coin
        n -= coin
    }
}
