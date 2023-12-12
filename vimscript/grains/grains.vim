const s:SIZE = 64

function! Square(number) abort
    if a:number < 1 || a:number > s:SIZE
        throw 'square must be between 1 and ' .. s:SIZE
    endif

    return float2nr(pow(2, a:number - 1))
endfunction

function! Total() abort
    return float2nr(pow(2, s:SIZE) - 1)
endfunction
