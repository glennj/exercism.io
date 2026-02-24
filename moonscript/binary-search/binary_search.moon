{
  find: (list, target) ->
    {left, right} = {1, #list}

    while left <= right
      mid = (left + right) // 2

      if target == list[mid] then return mid

      if target < list[mid]
        right = mid - 1
      else
        left = mid + 1

    error 'value not in array'
}
