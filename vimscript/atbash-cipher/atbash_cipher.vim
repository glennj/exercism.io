" ------------------------------------------------------------
function! AtbashDecode(cipher) abort
    return a:cipher
            \ ->substitute('[^[:alnum:]]', '', 'g')
            \ ->tolower()
            \ ->tr('abcdefghijklmnopqrstuvwxyz', 'zyxwvutsrqponmlkjihgfedcba')
endfunction

" ------------------------------------------------------------
function! AtbashEncode(plaintext) abort
    return AtbashDecode(a:plaintext)->s:grouped()
endfunction

function! s:grouped(str)
    "let str = substitute(str, '.\{5\}', '& ', 'g')
    "return substitute(str, ' $', '', '')

    " :he split()
    " > If you want to keep the separator you can also use '\zs' at the end of the pattern
    return a:str->split('.\{5\}\zs')->join(' ')
endfunction
