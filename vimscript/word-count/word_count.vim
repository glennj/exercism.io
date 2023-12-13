function! WordCount(phrase) abort
    return a:phrase
            \ ->s:matchstrAll("[[:alnum:]']\\+")
            \ ->map({_, w -> w->trim("'")})
            \ ->map({_, w -> w->tolower()})
            \ ->reduce(function('s:incrFrequency'), {})
endfunction


" ref: https://stackoverflow.com/q/34056600/7552
function! s:matchstrAll(str, pat)
    let matches = []
    eval a:str->substitute(a:pat, '\= matches->add(submatch(0))', 'g')
    return matches
endfunction

function! s:incrFrequency(freq, word)
    let a:freq[a:word] = a:freq->get(a:word, 0) + 1
    return a:freq
endfunction
