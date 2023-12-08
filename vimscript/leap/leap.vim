function! LeapYear(year) abort
    let DivBy = {n, d -> n % d == 0}
    return a:year->DivBy(4) && (!a:year->DivBy(100) || a:year->DivBy(400))
endfunction
