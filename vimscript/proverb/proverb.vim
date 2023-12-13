const s:FMT = #{
           \   for: 'For want of a %s the %s was lost.',
           \   and: 'And all for the want of a %s.'
           \ }

function! Lines(strings) abort
    let len = len(a:strings)
    if len == 0
        return [] 
    else
        return range(len - 1)
                \ ->map({_, i -> printf(s:FMT.for, a:strings[i], a:strings[i + 1])})
                \ ->add(printf(s:FMT.and, a:strings[0]))
    endif
endfunction
