let s:ALLERGENS = ['eggs', 'peanuts', 'shellfish', 'strawberries', 'tomatoes', 'chocolate', 'pollen', 'cats']

function! s:isAllergic(score, idx, ...)
    return and(a:score >> a:idx, 1)
endfunction

function! List(score) abort
    let Partial = function("s:isAllergic", [a:score])
    return s:ALLERGENS->copy()->filter(Partial)
endfunction

function! AllergicTo(score, allergy) abort
    return List(a:score)->index(a:allergy) != -1
endfunction
