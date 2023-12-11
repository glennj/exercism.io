function! Raindrops(number) abort
  let result = ''
  if a:number % 3 == 0 | let result .= 'Pling' | endif
  if a:number % 5 == 0 | let result .= 'Plang' | endif
  if a:number % 7 == 0 | let result .= 'Plong' | endif
  if empty(result)     | let result = string(a:number) | endif
  return result
endfunction
