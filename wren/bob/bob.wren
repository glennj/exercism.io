import "./byte" for Byte

class Bob {
  static hey(message) { this.given(message).response }

  construct given(input) {
    _m = BobMessage.new(input)
  }

  response {
    if (_m.silent)               return "Fine. Be that way!"
    if (_m.yelling && _m.asking) return "Calm down, I know what I'm doing!"
    if (_m.yelling)              return "Whoa, chill out!"
    if (_m.asking)               return "Sure."
    return "Whatever."
  }
}

class BobMessage {
  construct new(string) {
    _str = string.trim()
  }

  silent  { _str.isEmpty }
  asking  { _str.endsWith("?") }
  yelling {
    var hasUpper = Byte.bytes(_str).any {|b| b.isUpper}
    var hasLower = Byte.bytes(_str).any {|b| b.isLower}
    return hasUpper && !hasLower
  }
}
