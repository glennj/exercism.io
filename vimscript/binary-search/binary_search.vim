function! Find(list, value) abort
    let i = 0
    let j = len(a:list) - 1
    while i <= j
        let mid = (i + j) / 2
        if a:value ==# a:list[mid]
            return mid
        elseif a:value <# a:list[mid]
            j = mid - 1
        else
            i = mid + 1
        endif
    endwhile

    throw 'value not in list'
endfunction
