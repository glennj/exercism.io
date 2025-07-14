function debug(str) { print str > "/dev/fd/3" }

{ gsub(/-/, "", $1) }

!/^[[:digit:]]{9}[[:digit:]X]$/ { print "false"; next }

{
    debug("matches")
    split($1, ds, "")
    sum = 0
    for (i = 1; i <= 9; i++) {
        sum += (11 - i) * ds[i]
        debug("" i ": " sum)
    }
    sum += ds[10] == "X" ? 10 : ds[10]
    debug(sum)

    print sum % 11 == 0 ? "true" : "false"
}
