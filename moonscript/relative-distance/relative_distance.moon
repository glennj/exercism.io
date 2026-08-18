-- this tree only maps a person to their parent
buildFamilyTree = (people) ->
  tree = {}
  for parent, children in pairs people
    for child in *children
      tree[child] = parent
  tree

ancestry = (tree, person) ->
  ancestors = {}
  getAncestry = (name) ->
    table.insert ancestors, 1, name
    if tree[name]
      getAncestry tree[name]
  getAncestry person
  ancestors


{
  degreeOfSeparation: (familyTree, personA, personB) ->
    tree = buildFamilyTree familyTree
    ancestors = {person, ancestry(tree, person) for person in *{personA, personB}}

    -- not related
    return if ancestors[personA][1] != ancestors[personB][1]

    -- remove common ancestors
    while ancestors[personA][1] == ancestors[personB][1]
      table.remove ancestors[personA], 1
      table.remove ancestors[personB], 1

    if     #ancestors[personA] == 0 then #ancestors[personB]
    elseif #ancestors[personB] == 0 then #ancestors[personA]
    else                                 #ancestors[personA] + #ancestors[personB] - 1
}
