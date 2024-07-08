{
    n = $1
    $0 = 1
    while (n > 0) {
        print
        for (i = NF + 1; i > 1; i--) {
            $i += $(i - 1)
        }
        n--
    }
}