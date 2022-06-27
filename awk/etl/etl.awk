#!/usr/bin/env gawk -f

BEGIN {
    FPAT = "[[:alpha:]]|[[:digit:]]{1,2}"
}
NF > 1 {
    for (i = 2; i <= NF; i++)
        score[tolower($i)] = $1
}
END {
    OFS = ","
    PROCINFO["sorted_in"] = "@ind_str_asc"
    for (c in score)
        print c, score[c]
}
