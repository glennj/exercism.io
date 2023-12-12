" Put the functions in a separate Dict.
" Then, when we create an "instance" using copy(), it's a *shallow* copy:
" each instance's functions will be the same references.

let s:customSetPrototype = {}

function s:customSetPrototype.Len()
  return len(self.values)
endfunction

function s:customSetPrototype.IsEmpty()
  return self.Len() == 0
endfunction

function s:customSetPrototype.Contains(elem)
  " we keep the set sorted: use binary search
  let i = 0
  let j = self.Len() - 1
  while i <= j
    let mid = (i + j) / 2
    if a:elem > self.values[mid]
      let i = mid + 1
    elseif a:elem < self.values[mid]
      let j = mid - 1
    else
      return 1
    endif
  endwhile
  return 0
endfunction

function s:customSetPrototype.Add(elem)
  "if !self.Contains(a:elem)
  "   eval self.values->add(a:elem)->sort()
  "endif

  " insert the elem in its sorted position in the list
  if self.IsEmpty() || a:elem > self.values[-1]
    eval self.values->add(a:elem)
    return
  else
    let i = 0
    while i < self.Len()
      if a:elem == self.values[i]
        return
      elseif a:elem < self.values[i]
        eval self.values->insert(a:elem, i)
        return
      endif
      let i += 1
    endwhile
  endif
endfunction

function s:customSetPrototype.IsSubset(other)
  for v in self.values
    if !a:other.Contains(v) | return 0 | endif
  endfor
  return 1
endfunction

function s:customSetPrototype.IsDisjoint(other)
  for v in self.values
    if a:other.Contains(v) | return 0 | endif
  endfor
  return 1
endfunction

function s:customSetPrototype.Intersection(other)
  let vs = self.values->copy()->filter({_, v -> a:other.Contains(v)})
  return Set(vs)
endfunction

function s:customSetPrototype.Difference(other)
  let vs = self.values->copy()->filter({_, v -> !a:other.Contains(v)})
  return Set(vs)
endfunction

function s:customSetPrototype.Union(other)
  let vs = self.values->copy()->extend(a:other.values)
  return Set(vs)
endfunction

lockvar! s:customSetPrototype

" ------------------------------------------------------------
function! Set(values) abort
  let o = s:customSetPrototype->copy()
  let o.values = []
  for v in a:values | eval o.Add(v) | endfor
  return o
endfunction
