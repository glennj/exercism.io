BEGIN { FS = "" }
      { for (i = 1; i <= NF; i++) annotate(NR, i, $i) }
END   { print_annotated(NR, NF) }


function annotate(x, y, char,     dx, dy) {
    if (char == ".") {
        # this may already have a value (from a previous mine).
        # _add_ zero, don't _assign_ zero
        annotated[x][y] += 0
    }
    else if (char == "*") {
        annotated[x][y] = 99
        # increment neighbours
        # it's OK to increment a mine, we'll handle that later
        for (dx = -1; dx <= 1; dx++)
            for (dy = -1; dy <= 1; dy++)
                annotated[x + dx][y + dy] += 1
    }
}


function print_annotated(height, width,     x, y, n, char) {
    for (x = 1; x <= height; x++) {
        for (y = 1; y <= width; y++) {
            n = annotated[x][y]
            if      (n ==  0) char = "."
            else if (n >= 99) char = "*"
            else              char = "" n
            printf "%s", char
        }
        print ""
    }
}
