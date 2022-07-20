#!/usr/bin/env gawk -f

@include "team"

BEGIN { FS = ";" }

NF != 3 {next}

{
    for (i = 1; i <= 2; i++) {
        if (!($i in teams)) {
            teams[$i] = 1
            team::init($i)
        }
    }
}

$3 == "win"  { team::win($1);  team::lose($2) }
$3 == "loss" { team::lose($1); team::win($2) }
$3 == "draw" { team::draw($1); team::draw($2) }

function compare_teams(t1, _1, t2, _2,    p1, p2) {
    p1 = team::points(t1)
    p2 = team::points(t2)
    if (p1 != p2) return -(p1 - p2)
    if (t1 < t2) return -1; else return t1 > t2
}

END {
    fmt = "%-30s | %2s | %2s | %2s | %2s | %2s\n"
    PROCINFO["sorted_in"] = "compare_teams"

    printf fmt, "Team", "MP", "W", "D", "L", "P"
    for (t in teams)
        printf fmt, t,  team::played(t), \
                        team::wins(t),   \
                        team::draws(t),  \
                        team::losses(t), \
                        team::points(t)
}
