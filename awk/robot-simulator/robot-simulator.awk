#!/usr/bin/env gawk -f

# These variables are initialized on the command line (using '-v'):
# - x
# - y
# - dir

BEGIN {
    if (!x) x = 0
    if (!y) y = 0
    status = 0
    switch (dir) {
        case "north": case "east": case "south": case "west": break
        case "": dir = "north"; break
        default: 
            print "invalid direction" > "/dev/stderr"
            status = 1
            exit
    }
}

$1 == "R" {
    switch (dir) {
        case "north": dir = "east";  break
        case "east":  dir = "south"; break
        case "south": dir = "west";  break
        case "west":  dir = "north"; break
    }
    next
}

$1 == "L" {
    switch (dir) {
        case "north": dir = "west";  break
        case "east":  dir = "north"; break
        case "south": dir = "east";  break
        case "west":  dir = "south"; break
    }
    next
}

$1 == "A" {
    switch (dir) {
        case "north": y++; break
        case "east":  x++; break
        case "south": y--; break
        case "west":  x--; break
    }
    next
}

{
    print "invalid instruction" > "/dev/stderr"
    status = 1
    exit
}
END {
    if (status == 0)
        print x, y, dir
    exit status
}
