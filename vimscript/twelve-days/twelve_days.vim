let s:Data = [
      \ {'ordinal': 'first',    'gift': 'a Partridge in a Pear Tree.'},
      \ {'ordinal': 'second',   'gift': 'two Turtle Doves,'},
      \ {'ordinal': 'third',    'gift': 'three French Hens,'},
      \ {'ordinal': 'fourth',   'gift': 'four Calling Birds,'},
      \ {'ordinal': 'fifth',    'gift': 'five Gold Rings,'},
      \ {'ordinal': 'sixth',    'gift': 'six Geese-a-Laying,'},
      \ {'ordinal': 'seventh',  'gift': 'seven Swans-a-Swimming,'},
      \ {'ordinal': 'eighth',   'gift': 'eight Maids-a-Milking,'},
      \ {'ordinal': 'ninth',    'gift': 'nine Ladies Dancing,'},
      \ {'ordinal': 'tenth',    'gift': 'ten Lords-a-Leaping,'},
      \ {'ordinal': 'eleventh', 'gift': 'eleven Pipers Piping,'},
      \ {'ordinal': 'twelfth',  'gift': 'twelve Drummers Drumming,'},
      \ ]

let s:Intro = 'On the %s day of Christmas my true love gave to me:'


function! Recite(startVerse, endVerse) abort
  return range(a:startVerse, a:endVerse)->map({_, i -> s:Verse(i)})->join('\n')
endfunction

function! s:Verse(n) abort
  let And = {gift, i -> (i == 0 && a:n > 1 ? 'and ' : '') .. gift}

  return range(a:n)
        \->reverse()
        \->reduce( {verse, i -> verse->add( s:Data[i].gift->And(i) )},
        \          [ printf(s:Intro, s:Data[a:n-1].ordinal) ] )
        \->join(' ')
endfunction
