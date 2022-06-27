#!/usr/bin/env gawk -f

BEGIN {FPAT = "[[:alpha:]]"}
{for (i = 1; i <= NF; i++) seen[toupper($i)] = 1}
END {print (length(seen) == 26 ? "true" : "false")}
