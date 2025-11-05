"
" Given a customer ame and a ticket number, return a formatted ticket message.
"
" Example:
"
" :echo Format('Maarten', 9)
" Maarten, you are the 9th customer we serve today. Thank you!
"
function! Format(name, number) abort
  return a:name . ", you are the " . Ordinal(a:number) . " customer we serve today. Thank you!"
endfunction

function! Ordinal(n) abort
  let ones = a:n % 10
  let tens = a:n % 100
  if ones == 1 && tens != 11 | return a:n .. "st" | endif
  if ones == 2 && tens != 12 | return a:n .. "nd" | endif
  if ones == 3 && tens != 13 | return a:n .. "rd" | endif
  return a:n .. "th"
endfunction
