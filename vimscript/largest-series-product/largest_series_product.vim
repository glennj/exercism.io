function! LargestProduct(digitString, span) abort
    if a:digitString =~ '\D' | return -1 | endif

    let digits = a:digitString->split('\zs')
    if a:span < 0 || a:span > len(digits) | return -1 | endif
    

    let Product = {offset -> digits->slice(offset, offset + a:span)
                                 \ ->reduce({p, d -> p * d}, 1)}

    return range(len(digits) - a:span + 1)
            \ ->map({_, offset -> Product(offset)})
            \ ->max()
endfunction
