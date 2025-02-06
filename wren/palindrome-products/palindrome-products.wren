import "./number-extension" for NumExt

class PalindromeProducts {
  static smallest(min, max) { this.new(min, max).products(min * min .. max * max) }
  static largest(min, max)  { this.new(min, max).products(max * max .. min * min) }

  construct new(min, max) {
    if (min > max) Fiber.abort("min must be <= max")
    _min = min
    _max = max
  }

  products(range) {
    for (n in range) {
      if (NumExt.isPalindrome(n)) {
        var fs = NumExt.factorsInRange(n, _min, _max)
        if (fs.count > 0) return PalindromeProductsResult.new(n, fs)
      }
    }
    return PalindromeProductsResult.new(null, [])
  }
}

class PalindromeProductsResult {
  construct new(number, factors) {
    _num = number
    _fs = factors
  }
  value { _num }
  factors { _fs }
}
