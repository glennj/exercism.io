"
" Given a diagram of plants and a student's name, return the plants
" they planted.
"
" Exanples:
"
"   :echo Plants("VVCG\nVVRC", "Bob")
"   ['clover', 'grass', 'radishes', 'clover']
"

let s:students = [
      \ 'Alice', 'Bob', 'Charlie', 'David', 'Eve', 'Fred', 
      \ 'Ginny', 'Harriet', 'Ileana', 'Joseph', 'Kincaid', 'Larry',
      \ ]

let s:flora = { 
      \ 'G': 'grass',
      \ 'C': 'clover',
      \ 'R': 'radishes',
      \ 'V': 'violets'
      \ }

function! Plants(diagram, student) abort
  let idx = s:students->index(a:student)
  return a:diagram
        \ ->split('\n')
        \ ->map({_, row -> row->strpart(2 * idx, 2)->split('\zs')})
        \ ->flatten()
        \ ->map({_, p -> s:flora[p]})
endfunction
