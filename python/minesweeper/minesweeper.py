def board(grid):
    # Validation
    for row in grid:
        if len(row) != len(grid[0]):
            raise ValueError('lines are unequal')
        if any(c not in " *" for c in row):
            raise ValueError('illegal character')

    # Convert to a matrix where 0 is a space and 9 is a bomb
    counts = [
        [0 if ch == ' ' else 9 for ch in row]
        for row in grid
    ]

    # Increment all the bomb neighbours
    for r in range(len(grid)):
        for c in range(len(grid[0])):
            if counts[r][c] == 9:
                incr_neighbours(counts, r, c)

    # Convert back to board but with counts
    return [
        ''.join(
            '*' if n == 9 else
            ' ' if n == 0 else str(n)
            for n in row
        )
        for row in counts
    ]


def incr_neighbours(counts, r, c):
    neighbours = (
        (r-1, c-1), (r-1, c), (r-1, c+1),
        (r,   c-1),           (r,   c+1),
        (r+1, c-1), (r+1, c), (r+1, c+1),
    )
    for x, y in neighbours:
        if 0 <= x < len(counts) and 0 <= y < len(counts[0]):
            counts[x][y] += (0 if counts[x][y] == 9 else 1)
