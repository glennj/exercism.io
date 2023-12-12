function! ToNANP(number) abort
    " remove valid non-digits
    let num = a:number->substitute('[[:blank:]().-]', '', 'g')
    let num = num->substitute('^[+]', '', '')
    " remove valid country code
    if len(num) > 10 | let num = num->substitute('^1', '', '') | endif

    " errors: non-digits; number length; invalid area code or exchange
    if num =~ '\D' || len(num) != 10 || num =~ '^[01]\|^...[01]'
        return ''
    endif

    return num
endfunction
