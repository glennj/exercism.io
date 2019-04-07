def saddle_points(matrix):
    if any(len(row) != len(matrix[0]) for row in matrix):
        raise ValueError('Irregular matrix.')

    row_maxima = list(map(max, matrix))
    col_minima = list(map(min, list(zip(*matrix))))

    return {
        (r+1, c+1)
        for r, row_max in enumerate(row_maxima)
        for c, col_min in enumerate(col_minima)
        if row_max == col_min
    }
