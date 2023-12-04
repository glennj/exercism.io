class Pangram {
  static mask(codepoint) {
    if (65 <= codepoint && codepoint <=  90) return 1 << (codepoint - 65)
    if (97 <= codepoint && codepoint <= 122) return 1 << (codepoint - 97)
    return 0
  }

  static isPangram(sentence) {
    return 0x3ffffff == sentence.codePoints.reduce(0) {|bitfield, cp|
      return bitfield | mask(cp)
    }
  }
}
