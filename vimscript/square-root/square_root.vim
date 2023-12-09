" Using the Binary numeral system implementation from
" https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_(base_2)

function! SquareRoot(number) abort
    let n = a:number
    " find b, the largest power of 4 <= n
    let b = float2nr(pow(4, floor(log(n) / log(4))))
    let x = 0

    while b != 0
        if n >= x + b
            let n = n - x - b
            let x = x / 2 + b
        else
            let x = x / 2
        endif
        let b = b / 4
    endwhile

    return x
endfunction
