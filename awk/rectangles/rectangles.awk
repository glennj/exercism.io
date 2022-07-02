#!/usr/bin/env gawk -f

BEGIN {
    count = 0
    FPAT = "."
    PROCINFO["sorted_in"] = "vertex_sort"
}
{
    for (i = 1; i <= NF; i++) {
        # items in a multidimensional array index are joined with SUBSEP variable
        board[NR, i] = $i
        if ($i == "+") vertices[NR, i] = 1
    }
}

function vsplit(vertex, coords) { split(vertex, coords, SUBSEP) }

END {
    for (top_left in vertices) {
        delete vr; delete vb
        vertices_right_of(top_left, vr)
        vertices_below(top_left, vb)
        for (top_right in vr) {
            vsplit(top_right, tr)
            for (bottom_left in vb) {
                vsplit(bottom_left, bl)
                if ((bl[1], tr[2]) in vertices)
                    if (is_rectangle(tr[1], bl[1], bl[2], tr[2]))
                        count++
            }
        }
    }
    print count
}

function vertices_right_of(vertex, result,    v, a, b) {
    vsplit(vertex, a)
    for (v in vertices) {
        vsplit(v, b)
        # same row but the column is greater
        if (a[1] == b[1] && a[2] < b[2])
            result[v] = 1
    }
}
function vertices_below(vertex, result,    v, a, b) {
    vsplit(vertex, a)
    for (v in vertices) {
        vsplit(v, b)
        # same column but the row is greater
        if (a[2] == b[2] && a[1] < b[1])
            result[v] = 1
    }
}

# we're checking if the sides are complete
function is_rectangle(row1, row2, col1, col2,    r, c) {
    for (r = row1; r <= row2; r++) {
        if (board[r, col1] !~ /[+|]/) return 0
        if (board[r, col2] !~ /[+|]/) return 0
    }
    for (c = col1; c <= col2; c++) {
        if (board[row1, c] !~ /[+-]/) return 0
        if (board[row2, c] !~ /[+-]/) return 0
    }
    return 1
}
        
function vertex_sort(i1, v1, i2, v2,    vertex1, vertex2) {
    vsplit(i1, vertex1)
    vsplit(i2, vertex2)
    if (vertex1[1] != vertex2[1]) return vertex1[1] - vertex2[1]
    if (vertex1[2] < vertex2[2]) return -1; else return vertex1[2] > vertex2[2]
}
