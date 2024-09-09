###
`append` and `foldl` are the fundamental building blocks.
All the rest can be accomplished with them.

Builtin List functionality required: 
* for-in loops
* index addressing and assignment
* empty list contructor []

This will not be very performant: there's a lot of copying lists.
###

class ListOps
  @append: (list1, list2) ->
    result = []
    i = 0
    for elem in list1
      result[i++] = elem
    for elem in list2
      result[i++] = elem
    result

  @foldl: (list, fn, initial) ->
    accumulator = initial
    for elem in list
      accumulator = fn(accumulator, elem)
    accumulator

  @concat: (lists) -> 
    @foldl lists, ((acc, list) => @append acc, list), []

  @filter: (list, f) -> 
    @foldl list, ((acc, elem) => if f(elem) then @append(acc, [elem]) else acc), []

  @mylength: (list) -> 
    @foldl list, ((len, _) -> len + 1), 0

  @map: (list, fn) ->  
    @foldl list, ((acc, elem) => @append acc, [fn(elem)]), []

  @reverse: (list) -> 
    @foldl list, ((acc, elem) => @append [elem], acc), []

  @foldr: (list, fn, initial) -> 
    @foldl (@reverse list), fn, initial

module.exports = ListOps
