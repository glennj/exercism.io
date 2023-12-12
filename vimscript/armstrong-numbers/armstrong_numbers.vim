"
" Tests whether a number is equal to the sum of its digits, 
" each raised to the power of the overall number of digits.
"
function! IsArmstrongNumber(number) abort
    let [num, len, sum] = [a:number, len(a:number), 0]
    while num > 0
        let digit = num % 10
        let sum += pow(digit, len)
        let num /= 10
    endwhile
    return a:number == sum
endfunction
