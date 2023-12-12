function! FindAnagrams(candidates, subject) abort
    let ToKey = {s -> s->tolower()->split('\zs')->sort()}
    let sKey = ToKey(a:subject)
    return a:candidates->filter({_, c -> a:subject !=? c && sKey ==# ToKey(c)})
endfunction
