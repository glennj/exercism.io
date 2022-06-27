#!/usr/bin/env gawk -f

BEGIN {
    # a word is all digits, or letters optionally followed by an apostrophe and more letters
    FPAT = "[[:digit:]]+|[[:alpha:]]+('[[:alpha:]]+)?"
    OFS = ": "
    PROCINFO["sorted_in"] = "@ind_str_asc"
}
{
    delete count
    for (i = 1; i <= NF; i++) count[tolower($i)]++
    for (word in count) print word, count[word]
}
