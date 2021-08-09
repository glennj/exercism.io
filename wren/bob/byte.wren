// A helper class for working with ASCII bytes
class Byte {
  construct new(byte) {_byte = byte}

  isUpper {65 <= _byte && _byte <= 90}
  isLower {97 <= _byte && _byte <= 122}
  isAlpha {isUpper || isLower}
  isApostrophe {_byte == 39}

  upcase   {isLower ? this.type.new(_byte - 32) : this}
  downcase {isUpper ? this.type.new(_byte + 32) : this}
  toString {String.fromByte(_byte)}

  // return a sequence of Bytes of a String
  static bytes(str) { str.bytes.map {|b| this.new(b)} }
}
