function! EggCount(num) abort
    let c = 0
    let n = a:num
    while n > 0
        let c += and(n, 1)
        let n = s:lshift(n, 1)
    endwhile
    return c
endfunction

" ------------------------------------------------------------
" missing bitwise shift functions
"
function! s:lshift(number, bits)
    let [n, b] = [a:number, a:bits]
    while b > 0
        let n /= 2
        let b -= 1
    endwhile
    return n
endfunction
