function! Darts(x, y) abort
  let dist = sqrt(a:x * a:x + a:y * a:y)

  if     dist <=  1 | return 10
  elseif dist <=  5 | return  5
  elseif dist <= 10 | return  1
  else              | return  0
  endif
endfunction
