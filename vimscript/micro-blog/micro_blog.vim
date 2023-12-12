"scriptencoding utf-8

function! Truncate(string) abort
    return a:string->slice(0, 5)
endfunction
