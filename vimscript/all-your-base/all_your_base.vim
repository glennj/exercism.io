function! Rebase(digits, inputBase, outputBase) abort
    call s:assert(a:inputBase >= 2, 'input base must be >= 2')
    call s:assert(a:outputBase >= 2, 'output base must be >= 2')

    let decimal = 0
    for digit in a:digits
        call s:assert(0 <= digit && digit < a:inputBase, 'all digits must satisfy 0 <= d < input base')
        let decimal = decimal * a:inputBase + digit
    endfor

    let outputDigits = []
    while 1
        eval outputDigits->insert(decimal % a:outputBase)
        let decimal /= a:outputBase
        if decimal == 0 | break | endif
    endwhile
    return outputDigits
endfunction

function! s:assert(cond, msg)
    if !a:cond | throw a:msg | endif
endfunction
