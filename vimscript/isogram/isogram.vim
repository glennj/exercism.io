function! IsIsogram(phrase) abort
    let letters = {}
    for letter in a:phrase->tolower()->split('\zs')
        if letter =~ '[[:alpha:]]'
            if letters->has_key(letter)
                return 0
            endif
            let letters[letter] = 1
        endif
    endfor
    return 1

    " menketechnologie's clever solution
    " https://exercism.org/tracks/vimscript/exercises/isogram/solutions/menketechnologies
    ""  return a:phrase !~? '\(\a\).*\1'
endfunction
