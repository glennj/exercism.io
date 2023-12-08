"
" Find the difference between the square of the sum and the sum of the squares
" of the first N natural numbers.
"
" Examples:
"
"   :echo SquareOfSum(3)
"   36
"   :echo SumOfSquares(3)
"   14
"   :echo DifferenceOfSquares(3)
"   22
"
function! DifferenceOfSquares(number) abort
  return abs(SumOfSquares(a:number) - SquareOfSum(a:number))
endfunction

function! SquareOfSum(number) abort
  return s:sum(a:number, function("s:identity"), function("s:square"))
endfunction

function! SumOfSquares(number) abort
  return s:sum(a:number, function("s:square"), function("s:identity"))
endfunction

" ------------------------------------------------------------
function! s:identity(arg) abort
  return a:arg
endfunction

function! s:square(arg) abort
  return a:arg * a:arg
endfunction

function! s:sum(number, elemFunc, sumFunc) abort
  return range(1, a:number)
         \ ->reduce({sum, n -> sum + a:elemFunc(n)})
         \ ->a:sumFunc()
endfunction
