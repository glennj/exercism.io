" Change making algorithm from
" http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf
"
function! FindFewestCoins(coins, target) abort
  call s:Assert(a:target >= 0, "target can't be negative")

  let coins = a:coins->copy()->insert(v:null)  " algorithm uses 1-based indexing
  let S = s:Change(coins, a:target)
  return s:MakeChange(S, coins, a:target)
endfunction


function! s:Assert(condition, message) abort
  if ! a:condition
    throw a:message
  endif
endfunction

let s:MaxInt = (1 << v:numbersize - 1) - 2


" d - the list of denomination values
" n - the target amount
"
" C[p] is the minimum number of coins needed to make change for target p
" S[p] is the index of the first coin in the optimal set of change
"
function! s:Change(d, n) abort
  let C = [v:null]->repeat(a:n + 1)
  let S = [v:null]->repeat(a:n + 1)

  for p in range(1, a:n)
    let min = s:MaxInt
    let coin = v:null

    for i in range(1, len(a:d) - 1)
      if a:d[i] <= p
        if 1 + C[p - a:d[i]] < min
          let min = 1 + C[p - a:d[i]]
          let coin = i
        endif
      endif
    endfor

    let C[p] = min
    let S[p] = coin
  endfor

  return S
endfunction


function! s:MakeChange(S, d, n) abort
  let n = a:n
  let change = []

  while n > 0
    let idx = a:S[n]
    call s:Assert(idx isnot v:null, "can't make target with given coins")
    let coin = a:d[idx]
    eval change->add(coin)
    let n -= coin
  endwhile

  return change
endfunction
