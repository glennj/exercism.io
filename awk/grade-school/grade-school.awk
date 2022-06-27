#!/usr/bin/env gawk -f
@include "join"

BEGIN {
    FS = OFS = ","
    PROCINFO["sorted_in"] = "school_order"
}

!($1 in directory) { directory[$1] = $2 }

END {
    switch (action) {
        case "roster": print get_names(); break
        case "grade":  print get_names(grade); break
    }
}

function get_names(grade,    name, result, n) {
    for (name in directory) 
        if (typeof(grade) == "untyped" || grade == directory[name])
            result[++n] = name
    return join(result, 1, n, OFS)
}

function school_order(name1, grade1, name2, grade2) {
    if (grade1 != grade2) return grade1 - grade2
    if (name1 < name2) return -1; else return name1 > name2
}
