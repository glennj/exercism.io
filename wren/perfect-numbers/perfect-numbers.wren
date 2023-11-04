class PerfectNumbers {
  static aliquotSum(number) {
    var sum = (1..number.sqrt).
                where {|d| number % d == 0}.
                reduce(0) {|sum, f| 
                  var g = number / f
                  return sum + f + (f == g ? 0 : g)
                }
    return sum - number
  }

  static classify(number) {
    if (number < 1) Fiber.abort("Classification is only possible for positive integers.")

    var sum = aliquotSum(number)
    if (sum < number) return "deficient"
    if (sum > number) return "abundant"
    return "perfect" 
  }
}
