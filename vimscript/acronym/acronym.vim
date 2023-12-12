function! Abbreviate(phrase) abort
    " remove apostrophes within words
    let str = substitute(a:phrase, '\(\a\)''\a', '\1', 'g')
    " normalize punctuation and spaces
    let str = substitute(str, '[[:punct:][:space:]]\+', ' ', 'g')
    " isolate acronym letters, and uppercase them
    let str = substitute(str, '\<\(\a\)\a*', '\u\1', 'g')
    " remove whitespace
    return substitute(str, '[[:space:]]', '', 'g')
endfunction 
