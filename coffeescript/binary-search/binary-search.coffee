class BinarySearch
  constructor: (@values) ->

  find: (target, left = 0, right = @values.length - 1) ->
    throw new Error 'value not in array' if left > right
    mid = (left + right) // 2
    val = @values.get(mid)
    switch
      when target is val then return mid
      when target < val then @find(target, left, mid - 1)
      when target > val then @find(target, mid + 1, right)
      
      
module.exports = BinarySearch
