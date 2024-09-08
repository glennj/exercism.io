class Strain
  @keep: (lst, predicate) -> 
    elem for elem in lst when predicate(elem)

  @discard: (lst, predicate) ->
    @keep lst, (elem) -> not predicate(elem)

module.exports = Strain
