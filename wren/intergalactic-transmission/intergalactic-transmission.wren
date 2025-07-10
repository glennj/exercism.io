class IntergalacticTransmission {
  static transmitSequence(message) {
    var bits = toBits(message)
    var encoded = []

    while (bits.count >= 7) {
      var chunk = bits[0...7]
      encoded.add(bitsToByte(chunk + [parity(chunk)]))
      bits = bits[7..-1]
    }

    if (bits.count > 0) {
      while (bits.count < 7) { bits.add(0) }
      encoded.add(bitsToByte(bits + [parity(bits)]))
    }

    return encoded
  }

  static decodeMessage(message) {
    var decodedBits = []
    for (byte in message) {
      var bits = byteToBits(byte)
      if (parity(bits[0...7]) != bits[7]) {
        Fiber.abort("wrong parity")
      }
      decodedBits.addAll(bits[0...7])
    }
    
    var decoded = []
    while (decodedBits.count >= 8) {
      decoded.add(bitsToByte(decodedBits[0...8]))
      decodedBits = decodedBits[8..-1]
    }
    // the remaining bits are the padding bits that were 
    // added for the transmission

    return decoded
  }

  // ---------------------------------------------------

  static toBits(bytes) { bytes.reduce([]) {|acc, b| acc + byteToBits(b)}.toList }

  static byteToBits(byte) { (1..8).map {|i| (byte >> (8-i)) & 1}.toList }

  static bitsToByte(bits) { bits.reduce(0) {|acc, b| acc * 2 + b} }

  static parity(bits) { bits.where {|b| b == 1}.count & 1 }
}
