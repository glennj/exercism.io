function! SaddlePoints(matrix) abort
  if a:matrix->empty() | return [] | endif

  let [height, width] = [len(a:matrix), len(a:matrix[0])]
  let rows = a:matrix
  let cols = range(width)->map({_, j -> rows->mapnew({_, row -> row[j]}) })

  let rMax = rows->mapnew({_, row -> max(row)})
  let cMin = cols->mapnew({_, col -> min(col)})

  let coordinates = rows
        \->mapnew({i, _ -> cols->mapnew({j, _ -> {'x':i, 'y':j} }) })
        \->flatten()

  return coordinates
        \->filter({_, p -> rows[p.x][p.y] == rMax[p.x] && rows[p.x][p.y] == cMin[p.y]})
        \->map({_, p -> {'row': p.x + 1, 'column': p.y + 1} })
endfunction
