function! ColorCode(color) abort
  let code = index(Colors(), a:color)
  if code == -1 | throw 'Invalid color' | endif
  return code
endfunction


function! Colors() abort
  return ['black', 'brown', 'red', 'orange', 'yellow', 'green', 'blue', 'violet', 'grey', 'white']
endfunction
