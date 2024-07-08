NF {
    for (i = 2; i <= NF; i++) 
        printf "For want of a %s the %s was lost.\n", $(i-1), $i
    printf "And all for the want of a %s.\n", $1
}