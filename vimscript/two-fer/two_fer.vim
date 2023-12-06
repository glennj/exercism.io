function! TwoFer(...) abort
    let l:name = (a:0 == 0 || strlen(a:1) == 0) ? 'you' : a:1
    return 'One for ' . l:name . ', one for me.'
endfunction
