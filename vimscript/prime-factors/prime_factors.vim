"
" Given a number, return its prime factors.
"
" Examples:
"   :echo Factors(1)
"   []
"
"   :echo Factors(12)
"   [2, 2, 3]
"
function! Factors(value) abort
  let num = a:value
  let factors = []
  let divisor = 2

  while divisor * divisor <= num
    while num % divisor == 0
      eval factors->add(divisor)
      let num /= divisor
    endwhile
    let divisor += 1
  endwhile

  if num > 1 | eval factors->add(num) | endif
  return factors
endfunction
