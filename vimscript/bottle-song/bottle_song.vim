function! Recite(startBottles, takeDown) abort
    let result = []
    for i in range(a:takeDown)
        eval result->add('')->extend(s:verse(a:startBottles - i))
    endfor
    eval result->remove(0)
    return result
endfunction

function! s:verse(n)
    return [
        \    s:bottles(a:n) .. ',',
        \    s:bottles(a:n) .. ',',
        \    'And if one green bottle should accidentally fall,',
        \    'There''ll be ' .. tolower(s:bottles(a:n - 1)) .. '.',
        \  ]
endfunction

function! s:bottles(n)
    let numbers = ['No', 'One', 'Two', 'Three', 'Four', 'Five', 
                \  'Six', 'Seven', 'Eight', 'Nine', 'Ten']
    return printf('%s green bottle%s hanging on the wall',
                \ numbers[a:n],
                \ (a:n == 1 ? '' : 's'))
endfunction
