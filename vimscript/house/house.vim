const s:items = [
      \   ['house', 'Jack built.'],
      \   ['malt', 'lay in'],
      \   ['rat', 'ate'],
      \   ['cat', 'killed'],
      \   ['dog', 'worried'],
      \   ['cow with the crumpled horn', 'tossed'],
      \   ['maiden all forlorn', 'milked'],
      \   ['man all tattered and torn', 'kissed'],
      \   ['priest all shaven and shorn', 'married'],
      \   ['rooster that crowed in the morn', 'woke'],
      \   ['farmer sowing his corn', 'kept'],
      \   ['horse and the hound and the horn', 'belonged to'],
      \ ]
  
function! s:verse(n)
    return range(a:n)->reverse()->reduce(
    \   {v, i -> printf('%s the %s that %s', v, s:items[i][0], s:items[i][1])},
    \   'This is')
endfunction

function! ReciteVerses(startVerse, endVerse) abort
    return range(a:startVerse, a:endVerse)->map({_, n -> s:verse(n)})
endfunction
