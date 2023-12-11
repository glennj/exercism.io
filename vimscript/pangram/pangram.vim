function! IsPangram(sentence) abort
    let bitfield = 0
    for char in a:sentence->split('\zs')
        if char =~ '\a'
            let offset = char2nr(toupper(char)) - 65
            let bitfield = or(bitfield, 1 << offset)
        endif
    endfor
    return bitfield == 0x3FFFFFF
endfunction
