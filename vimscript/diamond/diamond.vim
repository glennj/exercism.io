let s:A_val = char2nr('A')

function! Rows(letter) abort
  let n = char2nr(a:letter) - s:A_val + 1

  function! Row(i) closure
    let half_row = [' ']->repeat(n)
    let half_row[a:i] = nr2char(s:A_val + a:i)
    return half_row->slice(1)->reverse()->extend(half_row)->join('')
  endfunction

  let rows = range(n)->map({_, i -> Row(i)})

  return rows->slice(0, -1)->extend( rows->reverse() )->join('\n')
endfunction
