Classification = {
  equal: 'equal',
  sublist: 'sublist',
  superlist: 'superlist',
  unequal: 'unequal',
}

class Sublist
  @classify: (listOne, listTwo) ->
    return Classification.equal     if @isEqual listOne, listTwo
    return Classification.sublist   if @isSublist listOne, listTwo
    return Classification.superlist if @isSublist listTwo, listOne
    return Classification.unequal

  @isEqual: (a, b) ->
    a.length == b.length and a.every (e, i) -> e is b[i]

  @isSublist: (a, b) -> 
    for i in [0 .. b.length - a.length]
      return true if @isEqual a, b[i ... i + a.length]
    return false


module.exports = { Sublist, Classification }
