proc binarySearch*(a: openArray[int], val: int): int =
  var i = a.low
  var j = a.high

  while i <= j:
    var mid = (i + j) div 2
    case cmp(val, a[mid])
      of  1: i = mid + 1
      of -1: j = mid - 1
      else: return mid
  return -1

