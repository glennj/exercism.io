use context starter2024

import lists as L

provide: tick end

fun tick(matrix):
  in_range = lam({r; c}):
    ((0 <= r) and (r < matrix.length())) and
    ((0 <= c) and (c < matrix.get(r).length()))
  end

  count_neighbours = lam(i, j):
    [list: {-1;-1}, {-1;0}, {-1;1}, {0;-1}, {0;1}, {1;-1}, {1;0}, {1;1}]
      .map(lam({di; dj}): {i + di; j + dj} end)
      .filter(in_range)
      .foldl(lam({r; c}, count): count + matrix.get(r).get(c) end, 0)
  end

  process_row = lam(i, row): 
    process_cell = lam(j, _):
      n = count_neighbours(i, j)
      ask:
        | n == 3 then: 1
        | n == 2 then: matrix.get(i).get(j)
        | otherwise: 0
      end
    end
    L.map_n(process_cell, 0, row)
  end

  L.map_n(process_row, 0, matrix)
end

