function collatz(n, steps) {
    if (n == 1)     return steps
    if (n % 2 == 0) return collatz(n / 2, steps + 1)
    else            return collatz(3 * n + 1, steps + 1)
}

$1 < 1 {
    print "Error: Only positive numbers are allowed" > "/dev/stderr"
    exit 1
}
{
    print collatz($1, 0)
}
