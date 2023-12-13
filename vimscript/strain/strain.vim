"
" Given a list and a predicate working on each element, returns a new list
" of those elements satisfying the predicate
"
function! Keep(list, predicate) abort
    let result = []
    for elem in a:list
        if a:predicate(elem)
            eval result->add(elem)
        endif
    endfor
    return result
endfunction

"
" Given a list and a predicate working on each element, returns a new list
" of those elements not satisfying the predicate
"
function! Discard(list, predicate) abort
    return Keep(a:list, {x -> !a:predicate(x)})
endfunction
