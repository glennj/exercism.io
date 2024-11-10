const s:SIZE = 64

execute 'source' expand('<sfile>:p:h') .. '/string_arithmetic.vim'

function! Square(number) abort
    if a:number < 1 || a:number > s:SIZE
        throw 'square must be between 1 and ' .. s:SIZE
    endif
    return StringPow('2', a:number - 1)
endfunction

function! Total() abort
    return StringPow('2', s:SIZE)->StringSubtract('1')
endfunction
