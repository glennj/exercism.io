var Accumulate = Fn.new { |list, fn|
  var result = []
  for (elem in list) {
    result.add(fn.call(elem))
  }
  return result
}
