class CryptoSquare {
  static ciphertext(plaintext) {

    var chars = plaintext.codePoints
      .where {|cp| (65..90).contains(cp) || (97..122).contains(cp) || (48..57).contains(cp)}
      .map {|cp| (65..90).contains(cp) ? cp + 32 : cp}
      .toList

    var segmentLength = chars.count.sqrt.ceil

    var segments = []
    while (chars.count > 0) {
      var segment = chars.take(segmentLength).toList
      while (segment.count < segmentLength) segment.add(32)
      segments.add(segment)
      chars = chars.skip(segmentLength).toList
    }

    return (0...segmentLength).reduce([]) {|transposed, i|
      var chunk = segments.map {|seg| String.fromCodePoint(seg[i])}
      transposed.add(chunk.join())
      return transposed
    }.join(" ")
  }
}
