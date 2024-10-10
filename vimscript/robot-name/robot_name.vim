"
" Creates a robot with a random name and a reset method.
"

" a set of assigned names
" TODO handling for all-names-assigned.
let s:names = {}

function! Create() abort
  let robot = {}

  function robot.Reset() dict
    let self.name = s:name()
    return self
  endfunction

  return robot.Reset()
endfunction


function! s:name() abort
  let Letter = { -> (65 + rand() % 26)->nr2char() }
  let Digits = { -> (rand() % 1000)->printf('%03d') }

  while 1
    let name = Letter() .. Letter() .. Digits()
    if !s:names->has_key(name) | break | endif
  endwhile

  let s:names[name] = 1
  return name
endfunction
