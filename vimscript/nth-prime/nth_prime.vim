"
" Given a number n, determine what the nth prime is.
"
" Example:
"
"   :echo Prime(6)
"   13
"
"   :echo Prime(10001)
"   104743
"
" I usually implement this maintaining a list of known
" primes and using that to determine if a number is prime.
" This is too slow in Vim.
"
function! Prime(number) abort
  if a:number < 1 | throw 'there is no zeroth prime' | endif
  if a:number == 1 | return 2 | endif

  let idx = 2
  let prime = 3

  while idx < a:number
    let prime += 2
    if IsPrime(prime) 
      let idx += 1
    endif
  endwhile

  return prime
endfunction

function! IsPrime(n)
  if a:n < 2 | return 0 | endif
  if a:n == 2 | return 1 | endif
  if a:n % 2 == 0 | return 0 | endif

  let divisor = 3
  let limit = a:n->sqrt()->float2nr()
  while divisor <= limit
    if a:n % divisor == 0 | return 0 | endif
    let divisor += 2
  endwhile

  return 1
endfunction
