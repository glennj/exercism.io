const s:ACTIONS = ['wink', 'double blink', 'close your eyes', 'jump']

function! Commands(binary) abort
    let endIdx = a:binary->len() - 1
    let actions = s:ACTIONS->copy()->filter({i, _ -> a:binary[endIdx - i] == '1'})
    return a:binary[0] == '1' ? actions->reverse() : actions
endfunction
