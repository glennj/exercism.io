class CustomSet
  constructor: (values) ->
    @values = []
    @add v for v in values

  empty: -> 
    @values.length == 0

  contains: (value) ->
    value in @values

  subset: (other) -> 
    @values.every (v) -> other.contains v

  disjoint: (other) -> 
    @intersection(other).empty()

  equal: (other) -> 
    @subset(other) and other.subset(this)

  add: (value) ->
    i = 0
    while i < @values.length
      return if @values[i] == value
      break if @values[i] > value
      i++
    @values.splice(i, 0, value)

  intersection: (other) ->
    new CustomSet(@values.filter (v) -> other.contains v)    

  difference: (other) ->
    new CustomSet(@values.filter (v) -> not other.contains v)

  union: (other) ->
    new CustomSet(@values.concat other.values)

    
module.exports = CustomSet
