const s:DNA = 'GCTA'
const s:RNA = 'CGAU'

function! ToRna(strand) abort
    if a:strand =~ '[^' . s:DNA . ']' | return '' | endif 
    return a:strand->tr(s:DNA, s:RNA)
endfunction
