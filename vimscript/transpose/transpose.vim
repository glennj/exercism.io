"
" Transpose the matrix represented by a string so rows become columns
" and columns become rows.
"
" Example:
"
"   :echo Transpose("ABC\nDEF")
"   AD
"   BE
"   CF
"
function! Transpose(text) abort
  let lines = a:text->split("\n")
  let max_wid = lines->mapnew({_, line -> strlen(line)})->max()
  let padded = lines->mapnew({_, line -> printf('%-*s', max_wid, line)})
  let transposed = range(max_wid)->map({i, _ -> 
                 \    padded->mapnew({_, line -> line[i]})
                 \          ->join('')
                 \          ->trim(' ', 2)
                 \ })

  " from the bottom up, each trimmed line should
  " not be shorter than the previous
  let len = -1
  for i in range(len(transposed))->reverse()
    let len = max([len, strlen(transposed[i])])
    let transposed[i] = printf('%-*s', len, transposed[i])
  endfor
  return transposed->join("\n")
endfunction
