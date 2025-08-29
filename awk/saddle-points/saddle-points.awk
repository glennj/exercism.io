# simplified thanks to rabestro's solution

function max(a, b) { return a > b ? a : b }
function min(a, b) { return a < b ? a : b }

{
    for (i = 1; i <= NF; i++) {
        rowmax[NR] = i == 1 ? $1 : max(rowmax[NR], $i)
        colmin[i] = NR == 1 ? $i : min(colmin[i], $i)
    }
}

END {
    for (r = 1; r <= NR; r++)
        for (c = 1; c <= NF; c++)
            if (rowmax[r] == colmin[c])
                print r, c
}
