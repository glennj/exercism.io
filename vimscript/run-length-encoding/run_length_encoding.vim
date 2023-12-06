function! Encode(string) abort
  " note to self: \C forces ignorecase off for the pattern
  return substitute(a:string, '\C\(.\)\1\+', '\=string(strlen(submatch(0))) . submatch(1)', 'g')
endfunction

function! Decode(string) abort
  return substitute(a:string, '\(\d\+\)\(\D\)', '\=repeat(submatch(2), 0 + submatch(1))', 'g')
endfunction
