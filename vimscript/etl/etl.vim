"
" We are going to do the Transform step of an Extract-Transform-Load.
"
" Example:
"
"   :echo Transform({'1': ['A', 'B'], '2': ['C']})
"   {'a': 1, 'b': 1, 'c': 2}
"

function! Transform(scores) abort
    let result = {}
    for [score, letters] in items(a:scores)
        let s = str2nr(score)
        for letter in letters
            let result[tolower(letter)] = s
        endfor
    endfor
    return result
endfunction
