function annotate(minefield)
    result = []

    for (r, row) ∈ pairs(IndexLinear(), minefield)
        for c ∈ 1:length(row)
            if minefield[r][c] == '*'
                continue
            end

            count = 0
            for _r ∈ r-1:r+1
                if _r < 1 || _r > length(minefield)
                    continue
                end

                for _c ∈ c-1:c+1
                    if _c < 1 || _c > length(row) || (_r == r && _c == c)
                        continue
                    end
                    if minefield[_r][_c] == '*'
                        count += 1
                    end
                end
            end

            if count > 0
                row = row[1:c-1] * string(count) * row[c+1:end]
            end
        end

        push!(result, row)
    end

    return result
end

"""
nice community solution

https://exercism.io/tracks/julia/exercises/minesweeper/solutions/2c776b090173426eb160cea17a85e536
"""
