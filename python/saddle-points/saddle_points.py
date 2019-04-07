def saddle_points(matrix):
    if any(len(row) != len(matrix[0]) for row in matrix):
        raise ValueError('Irregular matrix.')

    rows = matrix
    cols = list(zip(*matrix))
    return {
        (r+1, c+1)
        for r in range(len(rows))
        for c in range(len(cols))
        if rows[r][c] == max(rows[r]) and
           rows[r][c] == min(cols[c])
    }
