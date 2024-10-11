"
" Compute Pascal's triangle up to a given number of rows.
"
" Examples:
"
"   :echo Rows(4)
"   [[1], [1, 1], [1, 2, 1], [1, 3, 3, 1]
"
function! Rows(count) abort
  let triangle = []
  let i = 0
  while i < a:count
    let row = [1]->repeat(i+1)
    let j = 1
    while j < i
      let row[j] = triangle[i-1][j-1] + triangle[i-1][j]
      let j += 1
    endwhile
    eval triangle->add(row)
    let i += 1
  endwhile
  return triangle
endfunction
