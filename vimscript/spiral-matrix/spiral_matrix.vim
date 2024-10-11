" Generate a square matrix of natural numbers,
" starting from 1 in the top-left corner and incrementing
" in a clockwise spiral order.
"
function! SpiralMatrix(n)
  let m = range(1, a:n)->map({-> [-1]->repeat(a:n)})
  let [x, y] = [0, 0]
  let [dx, dy] = [0, 1]

  for i in range(1, a:n * a:n)
    let m[x][y] = i
    if x+dx == a:n || y+dy == a:n || y+dy < 0 || m[x+dx][y+dy] != -1
      let [dx, dy] = [dy, -dx]
    endif
    let [x, y] = [x + dx, y + dy]
  endfor

  return m
endfunction

