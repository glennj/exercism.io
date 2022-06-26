#!/usr/bin/env gawk -f

BEGIN {
    # defining what constitutes a "word" makes this easy:
    # starts with a letter and contains letters and apostrophes
    FPAT = "[[:alpha:]][[:alpha:]']*"
}
{
    acronym = ""
    for (i = 1; i <= NF; i++)
        acronym = acronym substr($i, 1, 1)
    print toupper(acronym)
}
