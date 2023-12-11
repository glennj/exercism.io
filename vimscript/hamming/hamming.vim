function! Distance(s1, s2)
    if len(a:s1) != len(a:s2)
        throw 'left and right strands must be of equal length'
    endif
    return range(len(a:s1))
         \ ->reduce({dist, i -> dist + (a:s1[i] != a:s2[i])}, 0)
endfunction
