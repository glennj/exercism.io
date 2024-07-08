BEGIN {
    FS = ","
}

{
    reading1 = 10 * $3 + $4
    reading2 = 10 * $5 + $6
    printf "#%d, %s = %d\n", $1, $2, (reading1 + reading2) / 2
}
