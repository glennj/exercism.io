const s:mapping = [ #{arabic: 1000, roman:  'M'},
                \   #{arabic:  900, roman: 'CM'},
                \   #{arabic:  500, roman:  'D'},
                \   #{arabic:  400, roman: 'CD'},
                \   #{arabic:  100, roman:  'C'},
                \   #{arabic:   90, roman: 'XC'},
                \   #{arabic:   50, roman:  'L'},
                \   #{arabic:   40, roman: 'XL'},
                \   #{arabic:   10, roman:  'X'},
                \   #{arabic:    9, roman: 'IX'},
                \   #{arabic:    5, roman:  'V'},
                \   #{arabic:    4, roman: 'IV'},
                \   #{arabic:    1, roman:  'I'},
                \ ]

function! ToRoman(number) abort
    let n = a:number
    let r = ''
    for i in range(len(s:mapping))
        if n == 0 | break | endif
        while n >= s:mapping[i].arabic
            let n -= s:mapping[i].arabic
            let r .= s:mapping[i].roman
        endwhile
    endfor
    return r
endfunction
