class BinarySearch
  constructor: (@values) ->

  find: (value, left = 0, right = @values.length - 1) ->
    throw new Error 'value not in array' if left > right
    mid = (left + right) // 2
    switch
      when value is @values[mid] then return mid
      when value < @values[mid] then @find(value, left, mid - 1)
      when value > @values[mid] then @find(value, mid + 1, right)
      
      
module.exports = BinarySearch
