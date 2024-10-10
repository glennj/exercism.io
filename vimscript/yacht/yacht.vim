""
"" Calculate the score for a given Yacht category and five dice
""
"" Example:
""
"" :echo Score("yacht", [5, 5, 5, 5, 5])
"" 50
""
function! Score(category, dice) abort
  if     a:category ==# 'ones'            | return s:single(a:dice, 1)
  elseif a:category ==# 'twos'            | return s:single(a:dice, 2)
  elseif a:category ==# 'threes'          | return s:single(a:dice, 3)
  elseif a:category ==# 'fours'           | return s:single(a:dice, 4)
  elseif a:category ==# 'fives'           | return s:single(a:dice, 5)
  elseif a:category ==# 'sixes'           | return s:single(a:dice, 6)
  elseif a:category ==# 'full house'      | return s:fullHouse(a:dice->sort('n'))
  elseif a:category ==# 'four of a kind'  | return s:fourOfAKind(a:dice->sort('n'))
  elseif a:category ==# 'little straight' | return s:straight(a:dice->sort('n'), 1, 5)
  elseif a:category ==# 'big straight'    | return s:straight(a:dice->sort('n'), 2, 6)
  elseif a:category ==# 'yacht'           | return s:yacht(a:dice->sort('n'))
  elseif a:category ==# 'choice'          | return a:dice->reduce({sum, die -> sum + die})
  else                                    | return 0
  endif
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:single(dice, die)
  return a:die * a:dice->filter({_, d -> d == a:die})->len()
endfunction

function! s:fullHouse(dice)
  return !Score('yacht', a:dice) && 
        \ ((a:dice[0] == a:dice[1] && a:dice[2] == a:dice[4]) ||
        \  (a:dice[0] == a:dice[2] && a:dice[3] == a:dice[4]))
        \ ? Score('choice', a:dice)
        \ : 0
endfunction 

function! s:fourOfAKind(dice)
  return (a:dice[0] == a:dice[3] || a:dice[1] == a:dice[4])
        \ ? a:dice[2] * 4
        \ : 0
endfunction

function! s:straight(dice, start, end)
  return a:dice == range(a:start, a:end) ? 30 : 0
endfunction

function! s:yacht(dice)
  return a:dice[0] == a:dice[4] ? 50 : 0
endfunction
