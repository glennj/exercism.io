import "./byte" for Byte

class Atbash {
  static initialize() {
    // create the encryption mapping
    var alphabet = "abcdefghijklmnopqrstuvwxyz"
    var alpharev = alphabet[-1..0]
    __mapping = (0...alphabet.count).reduce({}) {|m, i|
      m[alphabet[i]] = alpharev[i]
      return m
    }
    "0123456789".each {|digit| __mapping[digit] = digit}
  }

  static decode(input) {
    return Byte.bytes(input)
      .where {|byte| byte.isAlpha || byte.isDigit}
      .map {|byte| byte.downcase.toString}
      .map {|char| __mapping[char]}
      .join()
  }

  static encode(input) { groupsOf(5, decode(input)) }

  static groupsOf(size, str) {
    var groups = []
    while (!str.isEmpty) {
      var len = size.min(str.count)
      groups.add(str.take(len).join())
      str = str.skip(len)
    }
    return groups.join(" ")
  }
}

Atbash.initialize()
