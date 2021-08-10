// A helper class for working with ASCII bytes
class Byte {
  construct new(byte) {_byte = byte}
  value {_byte}

  isUpper {65 <= _byte && _byte <= 90}
  isLower {97 <= _byte && _byte <= 122}
  isAlpha {isUpper || isLower}
  isApostrophe {_byte == 39}

  upcase   {isLower ? this.type.new(_byte - 32) : this}
  downcase {isUpper ? this.type.new(_byte + 32) : this}
  toString {String.fromByte(_byte)}

  // for sorting
  <(other) {value < other.value}

  // return a sequence of Bytes of a String
  static bytes(str) { str.bytes.map {|b| this.new(b)} }

  // a sequence of Bytes as a String
  static toString(seqBytes) { seqBytes.map{|b| b.toString}.join() }

  // some string helpers
  static upcase(str) { toString(bytes(str).map {|b| b.upcase}) }
  static downcase(str) { toString(bytes(str).map {|b| b.downcase}) }

}
