" Try to limit the number of builtin commands and list-functions used.
"   - for
"   - add()
"   - insert()
"
" Most of the list operations can be implemented with Foldl.
" This would be more concise if lambdas could hold more than just expressions.

" ------------------------------------------------------------
function! Foldl(list, f, initial) abort
    let result = a:initial
    for elem in a:list
        let result = a:f(result, elem)
    endfor
    return result
endfunction

" ------------------------------------------------------------
function! Append(list, list2) abort
    return Foldl(a:list2, function('s:appendFn'), a:list)
endfunction

function! s:appendFn(acc, elem)
    return a:acc->add(a:elem)
endfunction

" ------------------------------------------------------------
function! ReversalOf(list) abort
    return Foldl(a:list, function('s:prependFn'), [])
endfunction

function! s:prependFn(acc, elem)
    return a:acc->insert(a:elem)
endfunction

" ------------------------------------------------------------
function! Concat(lists) abort
    return Foldl(a:lists, function("Append"), [])
endfunction

" ------------------------------------------------------------
function! Length(list) abort
    return Foldl(a:list, {i -> i + 1}, 0)
endfunction

" ------------------------------------------------------------
function! Filter(list, f) abort
    return Foldl(a:list, function("s:filterFn", [a:f]), [])
endfunction

function! s:filterFn(f, acc, elem) abort
    return a:f(a:elem) ? a:acc->add(a:elem) : a:acc
endfunction

" ------------------------------------------------------------
function! Map(list, f) abort
    return Foldl(a:list, function('s:mapFn', [a:f]), [])
endfunction

function! s:mapFn(f, acc, elem)
    return a:acc->add(a:f(a:elem))
endfunction

" ------------------------------------------------------------
function! Foldr(list, f, initial) abort
    let result = a:initial
    for elem in ReversalOf(a:list)
        let result = a:f(elem, result)
    endfor
    return result
endfunction
