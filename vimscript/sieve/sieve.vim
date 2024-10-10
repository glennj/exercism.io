"
" Generate a list of primes between 2 and the specified limit
"
function! Primes(limit) abort
  if a:limit < 2  | return []  | endif
  if a:limit == 2 | return [2] | endif

  let candidates = [0, 0] + range(2, a:limit)

  function! MarkMultiples(p, step) closure
    " can't use a range here as start may be greater than end
    let multiple = a:p * a:p
    while multiple <= a:limit
      let candidates[multiple] = 0
      let multiple += a:step
    endwhile
  endfunction

  eval MarkMultiples(2, 2)
  for p in range(3, a:limit->sqrt()->float2nr(), 2)
    if candidates[p]
      eval MarkMultiples(p, 2 * p)
    endif
  endfor

  return candidates->filter({_, n -> n > 0})
endfunction
