
class Binary { 
  construct new(numberString) {
    _numStr = numberString
  }

  toDecimal {
    var result = 0
    for (char in _numStr) {
      var digit = Num.fromString(char)
      if (digit == null || digit > 1) return
      result = result << 1 | digit
    }
    return result
  }
}
