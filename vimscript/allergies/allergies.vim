const s:ALLERGENS = ['eggs', 'peanuts', 'shellfish', 'strawberries', 'tomatoes', 'chocolate', 'pollen', 'cats']

" for some reason, I can't use a lambda for this function
function! s:isAllergic(score, idx, ...)
    return and(a:score >> a:idx, 1)
endfunction

function! List(score) abort
    return s:ALLERGENS->copy()->filter(function("s:isAllergic", [a:score]))
endfunction

function! AllergicTo(score, allergy) abort
    return List(a:score)->index(a:allergy) != -1
endfunction
