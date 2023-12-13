function! Sublist(list1, list2) abort
    let [len1, len2] = [len(a:list1), len(a:list2)]

    if len1 == len2
        return s:testEqual(a:list1, a:list2) ? 'equal' : 'unequal'
    elseif len1 < len2
        return s:testSublist(a:list1, len1, a:list2, len2) ? 'sublist' : 'unequal'
    else
        return s:testSublist(a:list2, len2, a:list1, len1) ? 'superlist' : 'unequal'
    endif
endfunction

function! s:testEqual(l1, l2) abort
    for i in range(len(a:l1))
        if a:l1[i] != a:l2[i]
            return 0
        endif
    endfor
    return 1
endfunction

function! s:testSublist(shortList, lenS, longList, lenL) abort
    for i in range(a:lenL - a:lenS + 1)
        if s:testEqual(a:shortList, a:longList->slice(i, i + a:lenS))
            return 1
        endif
    endfor
    return 0
endfunction
