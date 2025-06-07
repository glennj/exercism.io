isEmpty = (array) -> array.length is 0


class RelativeDistance
  @degreesOfSeparation: (familyTree, personA, personB) ->
    new RelativeDistance(familyTree).degrees(personA, personB)


  constructor: (@familyTree) ->

  degrees: (a, b) ->
    pa = @pathToRoot a
    pb = @pathToRoot b

    # check cases for unrelated
    if isEmpty(pa) and isEmpty(pb)
      return -1
    if isEmpty(pa)
      return if pb[0] isnt a then -1 else pb.length
    if isEmpty(pb)
      return if pa[0] isnt b then -1 else pa.length
    if pa[0] isnt pb[0]
      return -1

    # remove common ancestors
    while not isEmpty(pa) and not isEmpty(pb) and pa[0] is pb[0]
      pa.shift()
      pb.shift()

    return pa.length + pb.length + 1

  pathToRoot: (name, path = []) ->
    parent = @parentOf name
    if not parent
      path
    else
      @pathToRoot parent, [parent, path...]

  parentOf: (name) ->
    for parent, children of @familyTree
      if children.includes name
        return parent
    return


module.exports = RelativeDistance
