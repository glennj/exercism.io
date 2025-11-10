function! DegreeOfSeparation(familyTree, personA, personB) abort
  let people = s:ParseTree(a:familyTree)

  let Lineage = {name, lineage -> name is v:null
          \ ? lineage
          \ : Lineage(people[name].parent, lineage->insert(name))}

  let [aLineage, bLineage] = [Lineage(a:personA, []), Lineage(a:personB, [])]
  let [aLen, bLen] = [len(aLineage), len(bLineage)]

  " find the length of the common prefix
  let common = 0
  while common < aLen && common < bLen && aLineage[common] == bLineage[common]
    let common += 1
  endwhile

  if common == 0    | return -1       | endif
  if common == aLen | return bLen - 1 | endif
  if common == bLen | return aLen - 1 | endif
  return aLen + bLen - 2 * common - 1
endfunction


function! s:ParseTree(familyTree) abort
  let people = {}

  for [name, children] in items(a:familyTree)
    if !people->has_key(name)
      let people[name] = {'name': name, 'parent': v:null}
    endif
    for child in children
      let people[child] = {'name': child, 'parent': name}
    endfor
  endfor

  return people
endfunction
