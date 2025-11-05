let s:FLOWER = 9
let s:BLANK  = 0


function Annotate(garden) abort
  if a:garden->empty() | return [] | endif

  let nrows = a:garden->len()
  let ncols = a:garden[0]->len()

  let Cell2Value = {_, c -> c == "*" ? s:FLOWER : s:BLANK }
  let Value2Cell = {_, v -> v == s:BLANK ? " " : (v == s:FLOWER ? "*" : string(v))}
  let Accumulate = {x, y -> s:AddNeighbours(annotated, x, y, nrows, ncols) }

  let annotated = a:garden->mapnew({_, row -> row->split('\zs')->map(Cell2Value)})

  return annotated
        \->map({i, row -> row->map({j, _ -> Accumulate(i, j) }) })
        \->map({_, row -> row->map(Value2Cell)->join("") })
endfunction


function! s:AddNeighbours(mtx, r, c, nrows, ncols) abort
  if a:mtx[a:r][a:c] == s:FLOWER | return s:FLOWER | endif

  let n = 0
  for d in [[-1,-1],[0,-1],[1,-1], [-1,0],[1,0], [-1,1],[0,1],[1,1]]
    let dr = a:r + d[0]
    let dc = a:c + d[1]
    if (0 <= dr && dr < a:nrows) && (0 <= dc && dc < a:ncols) && a:mtx[dr][dc] == s:FLOWER
      let n += 1
    endif
  endfor
  return n
endfunction

