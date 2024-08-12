#!/usr/bin/env gawk -f

BEGIN {
    # https://www.gnu.org/software/gawk/manual/html_node/Controlling-Scanning.html
    PROCINFO["sorted_in"] = "@ind_num_asc"

    sounds[3] = "i"; sounds[5] = "a"; sounds[7] = "o"
    drops = ""

    for (divisor in sounds)
        if (num % divisor == 0)
            drops = sprintf("%sPl%sng", drops, sounds[divisor])

    print (drops ? drops : num)
}
