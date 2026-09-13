use context starter2024

import arg-max, arg-min from math

provide-types *

provide: saddle-points end

data TreeLocation:
  | tree(row, column)
end

fun saddle-points(matrix):
  if is-empty(matrix):
    [list: ]
  else:
    rowIndices = range(0, matrix.length())
    colIndices = range(0, matrix.get(0).length())

    # find the MAX value for each row, and the MIN value for each column
    rowMaxima = matrix.map(lam(row): row.get(arg-max(row)) end)
    colMinima = colIndices.map(lam(colIdx):
      col = matrix.map(lam(row): row.get(colIdx) end)
      col.get(arg-min(col))
    end)
  
    is-saddle-point = lam({rowIdx; colIdx}):
      value = matrix.get(rowIdx).get(colIdx)
      (value == rowMaxima.get(rowIdx)) and (value == colMinima.get(colIdx))
    end

    # a list of tuples of all the locations in the matrix
    allLocations = rowIndices.foldl(
      lam(rowIdx, acc):
        rowLocations = colIndices.map(lam(colIdx): {rowIdx; colIdx} end)
        acc.append(rowLocations)
      end,
      [list: ]
    )

    allLocations
      .filter(is-saddle-point)
      .map(lam({r; c}): tree(r + 1, c + 1) end)
  end
end
