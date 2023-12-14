const s:ALLERGENS = ['eggs', 'peanuts', 'shellfish', 'strawberries', 'tomatoes', 'chocolate', 'pollen', 'cats']

" for some reason, I can't use a lambda for this function
function! s:isAllergic(score, idx, ...)
    "return and(a:score >> a:idx, 1)
    " bitwise-shift operators didn't show up until 8.2.5003
    " vimscript-test-runner using Ubuntu 20.04 which ships vim-gtk at version 8.2.3995
    return and(a:score / float2nr(pow(2, a:idx)), 1)
endfunction

function! List(score) abort
    return s:ALLERGENS->copy()->filter(function("s:isAllergic", [a:score]))
endfunction

function! AllergicTo(score, allergy) abort
    return List(a:score)->index(a:allergy) != -1
endfunction
