function! TwoFer(...) abort
    return printf('One for %s, one for me.', (a:0 > 0 && a:1 != '') ? a:1 : 'you')
endfunction
