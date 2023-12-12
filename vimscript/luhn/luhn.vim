const s:DIGIT_VALUE = [[0,1,2,3,4,5,6,7,8,9],
                    \  [0,2,4,6,8,1,3,5,7,9]]

function! IsValid(value) abort
    let [sum, double, n, i] = [0, 0, 0, len(a:value)]
    while i > 0
        let i -= 1
        let ch = a:value[i]
        if ch =~ '\s' | continue | endif
        if ch !~ '\d' | return 0 | endif
        let sum += s:DIGIT_VALUE[double][str2nr(ch)]
        let double = !double
        let n += 1
    endwhile
    return (sum == 0 && n == 1) ? 0 : sum % 10 == 0
endfunction
