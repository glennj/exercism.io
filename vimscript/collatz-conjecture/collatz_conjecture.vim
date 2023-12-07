if &maxfuncdepth < 1000
    let &l:maxfuncdepth = 1000
endif

function! Steps(...) abort
    if a:0 < 1 || a:1 < 1
        throw 'Only positive integers are allowed'
    elseif a:0 == 1
        return Steps(a:1, 0)
    elseif a:1 == 1
        return a:2
    else
        return Steps(NextCollatz(a:1), a:2 + 1)
    endif
endfunction

function! NextCollatz(n)
    return a:n % 2 == 0 ? a:n/2 : 3*a:n + 1
endfunction
