function! DegreeOfSeparation(familyTree, personA, personB) abort
  let parent = s:ParseTree(a:familyTree)

  let Lineage = {name, lineage -> name is v:null
          \ ? lineage
          \ : Lineage(parent[name], lineage->insert(name))}

  let [aLineage, bLineage] = [Lineage(a:personA, []), Lineage(a:personB, [])]
  let [aLen, bLen] = [len(aLineage), len(bLineage)]

  " find the length of the common prefix
  let common = 0
  while common < aLen && common < bLen && aLineage[common] == bLineage[common]
    let common += 1
  endwhile

  if common == 0    | return -1          | endif
  if common == aLen | return bLen - aLen | endif
  if common == bLen | return aLen - bLen | endif
  return aLen + bLen - 2 * common - 1
endfunction


" find the (one!) parent of each child in the family tree
function! s:ParseTree(familyTree) abort
  let parent = {}

  for [name, children] in items(a:familyTree)
    if !parent->has_key(name)
      let parent[name] = v:null
    endif
    for child in children
      let parent[child] = name
    endfor
  endfor

  return parent
endfunction
