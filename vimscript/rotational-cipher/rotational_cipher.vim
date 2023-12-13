function! Rotate(shiftKey, text) abort
    return a:text
            \ ->split('\zs')
            \ ->map({_, c -> s:rotateChar(c, a:shiftKey)})
            \ ->join('')
endfunction


function! s:rotateChar(char, shiftKey)
    if a:char =~ '\a'
        let offset = char2nr(a:char =~ '\u' ? 'A' : 'a')
        return nr2char((char2nr(a:char) - offset + a:shiftKey) % 26 + offset)
    else
        return a:char
    endif
endfunction
