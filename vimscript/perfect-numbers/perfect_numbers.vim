function! Classify(number) abort
    if a:number < 1
        throw "Classification is only possible for positive integers."
    endif

    let sum = s:aliquotSum(a:number)

    if     sum < a:number | return "deficient"
    elseif sum > a:number | return "abundant"
    else                  | return "perfect"
    endif
endfunction

function! s:aliquotSum(number)
    let factors = {}
    for i in range(1, float2nr(floor(sqrt(a:number))))
        if a:number % i == 0
            let factors[i] = 1
            let factors[a:number / i] = 1
        endif
    endfor
    return keys(factors)->reduce({sum, f -> sum + f}, 0) - a:number
endfunction
