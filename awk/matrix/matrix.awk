@namespace "matrix"

function read(filename, m,    nr, n, i) {
    nr = 0
    while ((getline line < filename) > 0) {
        nr++
        n = split(line, chars, " ")
        for (i = 1; i <= n; i++)
            m[nr, i] = chars[i]
    }
    close(filename)
    #pprint(m)
}

function row(m, r,    cell, result, sep) {
    PROCINFO["sorted_in"] = "matrix::row_sort"
    for (cell in m) {
        if (cell ~ "^" r SUBSEP) {
            result = result sep m[cell]
            sep = " "
        }
    }
    return result
}

function column(m, c,    sell, result, sep) {
    PROCINFO["sorted_in"] = "matrix::col_sort"
    for (cell in m) {
        if (cell ~ SUBSEP c "$") {
            result = result sep m[cell]
            sep = " "
        }
    }
    return result
}

#############################################################################
function row_sort(i1, v1, i2, v2) {
    split(i1, cell1, SUBSEP)
    split(i2, cell2, SUBSEP)
    if (cell1[1] != cell2[1]) return cell1[1] - cell2[1]
    if (cell1[2] < cell2[2]) return -1; else return cell1[2] > cell2[2]
}

function col_sort(i1, v1, i2, v2) {
    split(i1, cell1, SUBSEP)
    split(i2, cell2, SUBSEP)
    if (cell1[2] != cell2[2]) return cell1[2] - cell2[2]
    if (cell1[1] < cell2[1]) return -1; else return cell1[1] > cell2[1]
}
