def rows(row_count, triangle=[]):
    if row_count < 0:
        raise ValueError

    i = len(triangle)

    if i == row_count:
        return triangle

    row = [choose(i, j) for j in range(i + 1)]
    return rows(row_count, triangle + [row])


def choose(n, k):
    """
    This is the "n choose k" operation
    """

    return factorial(n) / (factorial(k) * factorial(n - k))


FACTORIALS = [1, 1]

def factorial(n):
    for i in range(len(FACTORIALS), n + 1):
        FACTORIALS.append( i * FACTORIALS[-1] )

    return FACTORIALS[n]
