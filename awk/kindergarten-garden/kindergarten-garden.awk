#!/usr/bin/env gawk -f

# These variables are initialized on the command line (using '-v'):
# - name

BEGIN {
    FPAT = ".."

    n = split("Alice Bob Charlie David Eve Fred Ginny Harriet Ileana Joseph Kincaid Larry", names, / /)
    for (i = 1; i <= n; i++) student[names[i]] = i

    plant["G"] = "grass"
    plant["C"] = "clover"
    plant["R"] = "radishes"
    plant["V"] = "violets"
}

{ for (i = 1; i <= NF; i++) plots[i] = plots[i] $i }

END {
    idx = student[name]
    n = split(plots[idx], plot, "")
    result = plant[plot[1]]
    for (i = 2; i <= n; i++) result = result " " plant[plot[i]]
    print result
}