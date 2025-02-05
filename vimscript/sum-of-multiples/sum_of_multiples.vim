function! Sum(factors, limit) abort
    let fs = a:factors->filter('v:val > 0')

    return range(1, a:limit - 1)
            \ ->filter({ _, n -> s:any(fs, { f -> n % f == 0 }) })
            \ ->reduce({ sum, n -> sum + n }, 0)
endfunction

function! s:any(list, predicate) abort
    for elem in a:list
        if a:predicate(elem)
            return 1
        endif
    endfor
    return 0
endfunction
