" Perform arithmetic operations on string operands.
"
" Exponentiation can be expressed as a sequence of multiplications.
" Multiplication can be expressed as a sequence of additions.

function! StringPow(number, exp) abort
    let result = '1'
    for _ in range(a:exp)
        let result = StringMul(result, a:number)
    endfor
    return result
endfunction

function! StringMul(x, y) abort
    " TODO optimization: iterate over the smaller number
    let result = '0'
    for _ in range(a:y)
        let result = StringAdd(result, a:x)
    endfor
    return result
endfunction

function! s:Digits(str) abort
    return a:str->split('\zs')->map({_, c -> str2nr(c)})
endfunction

function! StringAdd(a, b) abort
    let width = [a:a->strlen(), a:b->strlen()]->max()
    let a = printf('%0*s', width, a:a)->s:Digits()
    let b = printf('%0*s', width, a:b)->s:Digits()
    let result = []
    let carry = 0

    for i in range(width - 1, 0, -1)
        let c = carry + a[i] + b[i]
        let result = result->insert(c % 10, 0)
        let carry = c / 10
    endfor
    return result->insert(carry, 0)->join('')->trim('0', 1)
endfunction

function! StringSubtract(a, b) abort
    let width = [a:a->strlen(), a:b->strlen()]->max()
    let a = printf('%0*s', width, a:a)->s:Digits()
    let b = printf('%0*s', width, a:b)->s:Digits()

    let negative = 0
    if a->join('') < b->join('')
        let [a, b] = [b, a]
        let negative = 1
    endif

    let result = []
    for i in range(width - 1, 0, -1)
        if a[i] < b[i]
            call s:Borrow(a, i)
        endif
        let result = result->insert(a[i] - b[i], 0)
    endfor  
    return (negative ? '-' : '') .. result->join('')->trim('0', 1)
endfunction

function! s:Borrow(ary, idx) abort
    if a:idx > 0
        let a:ary[a:idx] += 10
        let a:ary[a:idx - 1] -= 1
        if a:ary[a:idx - 1] < 0
            call s:Borrow(a:ary, a:idx - 1)
        endif
    endif
endfunction
