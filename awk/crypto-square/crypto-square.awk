function ceil(x,    y) {
    y = int(x)
    return y == x ? y : y + 1
}

function pad(str, len) {
    return sprintf("%-*s", len, str)
}

{
    cleaned = gensub(/[^[:alnum:]]/, "", "g", tolower($0))
    $0 = ""
}

cleaned {
    # break string into segments
    size = ceil(sqrt(length(cleaned)))
    n = 0
    while (cleaned) {
        segments[++n] = substr(cleaned, 1, size)
        cleaned = substr(cleaned, size + 1)
    }
    segments[n] = pad(segments[n], size)

    # transpose
    for (i = 1; i <= n; i++)
        for (j = 1; j <= size; j++)
            $j = $j substr(segments[i], j, 1)
}

1
