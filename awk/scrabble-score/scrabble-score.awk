#!/usr/bin/env gawk -f

BEGIN {
    v["A"] = v["E"] = v["I"] = v["O"] = v["U"] = 1
    v["L"] = v["N"] = v["R"] = v["S"] = v["T"] = 1
    v["D"] = v["G"] = 2
    v["B"] = v["C"] = v["M"] = v["P"] = 3
    v["F"] = v["H"] = v["V"] = v["W"] = v["Y"] = 4
    v["K"] = 5
    v["J"] = v["X"] = 8
    v["Q"] = v["Z"] = 10

    OFS = ","
}

{
    score = 0
    input = toupper($0)
    n = split(input, chars, "")
    for (i = 1; i <= n; i++)
        score += v[chars[i]]
    print input, score
}
