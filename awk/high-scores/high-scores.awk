#!/usr/bin/env gawk -f

BEGIN { PROCINFO["sorted_in"] = "@val_num_desc" }

$1 ~ /^[[:digit:]]+/ {
    scores[++s] = $1
}

$1 == "list" {
    for (i = 1; i <= s; i++)
        print scores[i]
}

$1 == "latest" {
    print scores[s]
}

$1 == "personalBest" {
    for (i in scores) {
        print scores[i]
        break
    }
}

$1 == "personalTopThree" {
    count = 0
    for (i in scores) {
        print scores[i]
        if (++count == 3) break
    }
}
