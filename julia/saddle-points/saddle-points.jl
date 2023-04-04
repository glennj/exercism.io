function saddlepoints(M)
    if ndims(M) != 2
        return []
    end

    row_maxima = [maximum(row) for row in (M[r, :] for r in 1:size(M, 1))]
    col_minima = [minimum(col) for col in (M[:, c] for c in 1:size(M, 2))]

    is_saddle = (i, j) -> M[i,j] == row_maxima[i] && M[i,j] == col_minima[j]

    [(r, c) for r in 1:size(M,1), c in 1:size(M,2) if is_saddle(r, c)]
end
