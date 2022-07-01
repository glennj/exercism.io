#!/usr/bin/env gawk -f
@include "assert"

BEGIN {
    ocr[" _ :| |:|_|:"] = 0
    ocr["   :  |:  |:"] = 1
    ocr[" _ : _|:|_ :"] = 2
    ocr[" _ : _|: _|:"] = 3
    ocr["   :|_|:  |:"] = 4
    ocr[" _ :|_ : _|:"] = 5
    ocr[" _ :|_ :|_|:"] = 6
    ocr[" _ :  |:  |:"] = 7
    ocr[" _ :|_|:|_|:"] = 8
    ocr[" _ :|_|: _|:"] = 9
    status = 0
    FPAT = "[ _|]{3}"
}

function error(msg) {
    print msg > "/dev/stderr"
    status = 1
}

length % 3 != 0 { error("Number of input columns is not a multiple of three"); exit }

NR % 4 == 1 { n = int(NR / 4) }
NR % 4  > 0 { for (i = 1; i <= NF; i++) row[n][i] = row[n][i] $i ":" }

END {
    if (status == 0 && NR % 4 != 0) error("Number of input lines is not a multiple of four")
    if (status == 0) {
        for (i = 0; i <= n; i++) {
            number = number sep
            for (j = 1; j <= length(row[i]); j++)
                    number = number (row[i][j] in ocr ? ocr[row[i][j]] : "?")
            sep = ","
        }
        print number
    }
    exit status
}