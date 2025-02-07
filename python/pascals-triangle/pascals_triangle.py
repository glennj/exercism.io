from fact import choose

def rows(row_count, triangle=[]):
    if row_count < 0:
        raise ValueError

    n = len(triangle)

    if n == row_count:
        return triangle

    row = [choose(n, k) for k in range(n + 1)]
    return rows(row_count, triangle + [row])
