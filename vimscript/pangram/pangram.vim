function! IsPangram(sentence) abort
    let bitfield = 0
    for char in a:sentence->split('\zs')
        if char =~ '\a'
            let offset = char2nr(toupper(char)) - 65
            "let bitfield = or(bitfield, 1 << offset)
            " bitwise-shift operators didn't show up until 8.2.5003
            " vimscript-test-runner using Ubuntu 20.04 which ships vim-gtk at version 8.2.3995
            let bitfield = or(bitfield, float2nr(pow(2, offset)))
        endif
    endfor
    return bitfield == 0x3FFFFFF
endfunction
