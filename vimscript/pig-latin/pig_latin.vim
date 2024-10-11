" Given a phrase, translate it using Pig Latin rules. 

let s:patterns = [
      \ '^\(\)\(\%(xr\|yt\|[aeiou]\).*\)',
      \ '^\([^aeiou]*qu\)\(.*\)',
      \ '^\([^aeiouy]\+\)\(y.*\)',
      \ '^\([^aeiou]\+\)\(.*\)',
      \ ]

function! s:translate(word) abort
  for p in s:patterns
    let m = a:word->matchlist(p)
    if !m->empty()
      return m[2] .. m[1] .. 'ay'
    endif
  endfor
  return a:word
endfunction

function! Translate(phrase) abort
  return a:phrase
        \ ->split()
        \ ->map('s:translate(v:val)')
        \ ->join(' ')
endfunction
