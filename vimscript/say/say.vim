let s:Small = [
      \ 'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight',
      \ 'nine',  'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen',
      \ 'sixteen', 'seventeen', 'eighteen', 'nineteen' ]

let s:Tens = [
      \ '', '', 'twenty', 'thirty', 'forty', 'fifty', 
      \ 'sixty', 'seventy', 'eighty', 'ninety' ]

let s:Thousand = 1000
let s:Million  = 1000000
let s:Billion  = 1000000000
let s:Trillion = 1000000000000

" ------------------------------------------------------------
function! Say(number) abort
  if a:number < 0 || a:number >= s:Trillion | throw "input out of range" | endif

  if     a:number < 100        | return s:Simple(a:number)
  elseif a:number < s:Thousand | return s:Compound(a:number, 'hundred', 100)
  elseif a:number < s:Million  | return s:Compound(a:number, 'thousand', s:Thousand)
  elseif a:number < s:Billion  | return s:Compound(a:number, 'million', s:Million)
  else                         | return s:Compound(a:number, 'billion', s:Billion)
  endif
endfunction

" ------------------------------------------------------------
function! s:Simple(n) abort
  if a:n < len(s:Small) | return s:Small[a:n] | endif
  let [tens, ones] = a:n->s:Divmod(10)
  return s:Tens[tens] .. (ones == 0 ? '' : '-' .. s:Small[ones])
endfunction

function! s:Compound(n, word, base) abort
  let [quo, rem] = a:n->s:Divmod(a:base)
  return Say(quo) .. ' ' .. a:word .. (rem == 0 ? '' : ' ' .. Say(rem))
endfunction

function! s:Divmod(number, divisor) abort
  return [a:number / a:divisor, a:number % a:divisor]
endfunction
