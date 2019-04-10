from itertools import cycle


def spiral(size):
    matrix = [[None] * size for _ in range(size)]
    r, c = 0, 0

    deltas = cycle(((0,1), (1,0), (0,-1), (-1,0)))
    dr, dc = next(deltas)

    for i in range(size**2):
        matrix[r][c] = i+1
        if (
            not 0 <= r+dr < size or
            not 0 <= c+dc < size or
            matrix[r+dr][c+dc] is not None
        ):
            dr, dc = next(deltas)
        r += dr
        c += dc
    return matrix
