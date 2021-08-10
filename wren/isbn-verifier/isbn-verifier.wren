var IsDigit    = Fn.new {|char| (48..57).contains(char.bytes[0])}
var IsDigitOrX = Fn.new {|char| IsDigit.call(char) || char == "X"}

class ISBN {
  construct new(s) {
    _isbn = s.replace("-", "")
    _formatValid = _isbn.count == 10 &&
      _isbn[0..-2].all {|c| IsDigit.call(c)} &&
      IsDigitOrX.call(_isbn[-1])
  }

  isValid {
    if (!_formatValid) return false
    var m = 10
    var sum = _isbn.reduce(0) {|sum, char|
      var digit = (m == 1 && char == "X") ? 10 : Num.fromString(char)
      sum = sum + m * digit
      m = m - 1
      return sum
    }
    return sum % 11 == 0
  }
}
