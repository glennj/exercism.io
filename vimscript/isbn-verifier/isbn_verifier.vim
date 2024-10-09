"
" Returns 1 if the given ISBN-10 is valid, 0 otherwise.
"
" Example:
"
"   :echo IsValid('3-598-21508-8')
"   1
"
"   :echo IsValid('3-598-21508-9')
"   0
"
function! IsValid(isbn) abort
  let [sum, idx] = [0, 0]

  for char in a:isbn->split('\zs')
    if char == '-'
      continue
    elseif char =~ '\d'
      let sum += (10 - idx) * str2nr(char)
    elseif char ==# 'X' && idx == 9
      let sum += 10
    else
      return 0
    endif
    let idx += 1
  endfor

  return idx == 10 && sum % 11 == 0
endfunction
