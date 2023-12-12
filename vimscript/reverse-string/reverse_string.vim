function! Reverse(text) abort
    return a:text->split('\zs')->reduce({reversed, char -> char . reversed}, '')
endfunction
