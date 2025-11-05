function! Tick(matrix) abort
  let previous = a:matrix
  if previous->empty() | return [] | endif

  let nrows = previous->len()
  let ncols = previous[0]->len()

  function! CountNeighbours(r, c) closure
    return s:NEIGHBOURS
          \->mapnew({_, delta -> {'x': a:r + delta.x, 'y': a:c + delta.y}})
          \->filter({_, coord -> 0 <= coord.x && coord.x < nrows})
          \->filter({_, coord -> 0 <= coord.y && coord.y < ncols})
          \->reduce({sum, coord -> sum + previous[coord.x][coord.y]}, 0)
  endfunction

  function! NewValue(r, c) closure
    let n = CountNeighbours(a:r, a:c)
    return {3: 1, 2: previous[a:r][a:c]}->get(n, 0)
  endfunction

  return previous->mapnew({i, row -> row->mapnew({j, _ -> NewValue(i, j) }) })

endfunction


let s:NEIGHBOURS = [ {'x':-1,'y':-1}, {'x':0,'y':-1}, {'x':1,'y':-1},
                   \ {'x':-1,'y':0},                  {'x':1,'y':0},
                   \ {'x':-1,'y':1},  {'x':0,'y':1},  {'x':1,'y':1} ]
