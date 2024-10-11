" Returns an object representing a school roster, which can add a student, 
" list students in a grade, or list all students.
" Assume names are _case insensitive_.
"
function! GradeSchool() abort
  let school = {}
  let school.directory = []

  function! school.Add(name, grade) abort dict
    if -1 != self.directory->indexof({_, student -> student.name ==? a:name})
      return 0
    else
      eval self.directory->add({'name': a:name, 'grade': a:grade})
      return 1
    endif
  endfunction

  function! school.Roster() abort dict
    return self.directory->s:sortedNames()
  endfunction

  function! school.Grade(grade) abort dict
    return self.directory
          \ ->filter({_, student -> student.grade == a:grade})
          \ ->s:sortedNames()
  endfunction

  return school
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:sortedNames(studentList) abort
  return a:studentList
        \ ->sort('s:rosterSort')
        \ ->map({_, student -> student.name})
endfunction

function! s:rosterSort(a, b) abort
  let cmp = a:a.grade - a:b.grade
  return cmp != 0 ? cmp : s:strcmp(a:a.name, a:b.name)
endfunction

function! s:strcmp(a, b) abort
  if a:a <? a:b | return -1 | endif
  if a:a >? a:b | return  1 | endif
  return 0
endfunction
