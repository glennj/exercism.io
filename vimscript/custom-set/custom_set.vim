" Put the functions in a global Dict.
" Then when we create an "instance" using copy(), it's a shallow
" copy so that each instance's functions refer to the same ones.

let g:CustomSetPrototype = {}

function g:CustomSetPrototype.IsEmpty()
  return len(self.values) == 0
endfunction

function g:CustomSetPrototype.Contains(elem)
  " we keep the set sorted, so use binary search
  let i = 0
  let j = len(self.values) - 1
  while i <= j
    let mid = (i + j) / 2
    if self.values[mid] == a:elem
      return 1
    elseif self.values[mid] < a:elem
      let i = mid + 1
    else
      let j = mid - 1
    endif
  endwhile
  return 0
endfunction

function g:CustomSetPrototype.Add(elem)
  if !self.Contains(a:elem)
    let self.values = self.values->add(a:elem)->sort()
  endif
endfunction

function g:CustomSetPrototype.IsSubset(other)
  for v in self.values
    if !a:other.Contains(v) | return 0 | endif
  endfor
  return 1
endfunction

function g:CustomSetPrototype.IsDisjoint(other)
  for v in self.values
    if a:other.Contains(v) | return 0 | endif
  endfor
  return 1
endfunction

function g:CustomSetPrototype.Intersection(other)
  let vs = self.values->copy()->filter({_, v -> a:other.Contains(v)})
  return Set(vs)
endfunction

function g:CustomSetPrototype.Difference(other)
  let vs = self.values->copy()->filter({_, v -> !a:other.Contains(v)})
  return Set(vs)
endfunction

function g:CustomSetPrototype.Union(other)
  let vs = self.values->copy()->extend(a:other.values)
  return Set(vs)
endfunction

" ------------------------------------------------------------
function! Set(values) abort
  let o = g:CustomSetPrototype->copy()
  let o.values = []
  for v in a:values | eval o.Add(v) | endfor
  return o
endfunction
