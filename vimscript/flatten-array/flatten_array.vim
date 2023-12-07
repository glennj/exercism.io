function! Flatten(array) abort
    let flattened = []
    for elem in a:array
        if type(elem) == v:t_list
            call extend(flattened, Flatten(elem))
        elseif elem isnot v:null
            call add(flattened, elem)
        endif
    endfor
    return flattened
endfunction
