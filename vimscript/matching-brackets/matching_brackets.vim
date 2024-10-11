" Given a string containing brackets, determine if the brackets are balanced.
"

" map a close bracket to its open bracket
let s:brackets = { ')':'(', ']':'[', '}':'{', }

function! IsPaired(str) abort
  let stack = []
  for c in a:str->split('\zs')
    if s:brackets->has_key(c)
      if stack->len() == 0 || stack->remove(0) != s:brackets[c]
        " this closer's opener is not top of stack
        return 0
      endif
    elseif s:brackets->values()->index(c) != -1
      eval stack->insert(c)
    endif
  endfor
  return stack->len() == 0
endfunction

