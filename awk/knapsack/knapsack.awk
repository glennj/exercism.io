#!/usr/bin/env gawk -f

# Using the algorithm spelled out on Wikipedia

BEGIN {
    n = 0
}
match($0, /limit:([0-9]+)/, p) {
    W = p[1]
}
match($0, /weight:([0-9]+),value:([0-9]+)/, p) {
    n++
    w[n] = p[1]
    v[n] = p[2]
}

END {
    for (j = 0; j <= W; j++) m[0][j] = 0
    for (i = 0; i <= n; i++) m[i][0] = 0

    for (i = 1; i <= n; i++)
        for (j = 0; j <= W; j++)
            if (w[i] > j) 
                m[i][j] = m[i - 1][j]
            else
                m[i][j] = max(m[i - 1][j], m[i - 1][j - w[i]] + v[i])
    print m[n][W]
}

function max(a, b) {
    return a > b ? a : b
}
