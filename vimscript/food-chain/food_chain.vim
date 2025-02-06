function! Recite(endVerse, startVerse) abort
    let lyrics = s:verse(a:startVerse)
    for i in range(a:startVerse + 1, a:endVerse)
        eval lyrics->add( '' )
        eval lyrics->extend( s:verse(i) )
    endfor
    return lyrics
endfunction

" ------------------------------------------------------------
let s:Animals = ['', 'fly', 'spider', 'bird', 'cat', 'dog', 'goat', 'cow', 'horse']

let s:Reaction = #{ fly:    'I don''t know why she swallowed the fly. Perhaps she''ll die.'
               \  , spider: 'It wriggled and jiggled and tickled inside her.'
               \  , bird:   'How absurd to swallow a bird!'
               \  , cat:    'Imagine that, to swallow a cat!'
               \  , dog:    'What a hog, to swallow a dog!'
               \  , goat:   'Just opened her throat and swallowed a goat!'
               \  , cow:    'I don''t know how she swallowed a cow!'
               \  , horse:  'She''s dead, of course!'
               \  }

function! s:verse(n) abort
    let animal = s:Animals[a:n]
    let lines = ['I know an old lady who swallowed a ' .. animal .. '.']
    eval lines->add( s:Reaction[animal] )
    if animal != 'horse'
        for i in range(a:n - 1, 1, -1)
            eval lines->add( s:swallowed(i) )
        endfor
        if animal != 'fly'
            eval lines->add( s:Reaction['fly'] )
        endif
    endif
    return lines
endfunction

function! s:swallowed(n) abort
    let predator = s:Animals[a:n + 1]
    let prey = s:Animals[a:n]
    let line = 'She swallowed the ' .. predator .. ' to catch the ' .. prey
    if prey == 'spider'
        let line = line .. ' that ' .. s:Reaction['spider'][3:-2]
    endif
    return line .. '.'
endfunction
