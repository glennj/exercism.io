function! SaddlePoints(matrix) abort
  if a:matrix->empty() | return [] | endif

  "return s:SaddlePoints_functional(a:matrix)
  return s:SaddlePoints_fewer_iterations(a:matrix)
endfunction


" This version is clean and functional, but it has to iterate over the
" matrix 4 times before the `return` expression.
"
function! s:SaddlePoints_functional(matrix) abort
  let rows = a:matrix
  let cols = rows[0]->mapnew({i, _ -> rows->mapnew({_, row -> row[i]}) })

  let rMax = rows->mapnew({_, row -> max(row)})
  let cMin = cols->mapnew({_, col -> min(col)})

  let points = rows
        \->mapnew({i, _ -> cols->mapnew({j, _ -> {'x':i, 'y':j} }) })
        \->flatten()

  return points
        \->filter({_, p -> rows[p.x][p.y] == rMax[p.x] && rows[p.x][p.y] == cMin[p.y]})
        \->map({_, p -> {'row': p.x + 1, 'column': p.y + 1} })
endfunction


" This version is a little longer but more efficient: it only iterates over
" the matrix once.
"
function! s:SaddlePoints_fewer_iterations(matrix) abort
  let points = []
  let rMax = [v:numbermin]->repeat(len(a:matrix))
  let cMin = [v:numbermax]->repeat(len(a:matrix[0]))

  function! ProcessCell(x, y, value) closure
    call add(points, {'x': a:x, 'y': a:y})
    let rMax[a:x] = max([rMax[a:x], a:value])
    let cMin[a:y] = min([cMin[a:y], a:value])
  endfunction

  for [i, row] in items(a:matrix)
    for [j, v] in items(row)
      call ProcessCell(i, j, v)
    endfor
  endfor

  return points
        \->filter({_, p -> a:matrix[p.x][p.y] == rMax[p.x] && a:matrix[p.x][p.y] == cMin[p.y]})
        \->map({_, p -> {'row': p.x + 1, 'column': p.y + 1} })
endfunction
