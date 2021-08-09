import "./reverse-string" for StringUtil

var A = "A".bytes[0]

class Diamond {
  static rows(letter) { this.letter(letter).rows }

  construct letter(letter) {
    _max = letter.bytes[0]
  }

  size { _max - A + 1 }

  halfRow(byte) {
    var spaces = List.filled(size, " ")
    spaces[byte - A] = String.fromByte(byte)
    return spaces.join()
  }

  wholeRow(byte) {
    var rightHalf = halfRow(byte)
    return StringUtil.reverse(rightHalf.skip(1)) + rightHalf
  }

  rows {
    return (0...size).reduce(List.filled(2 * size - 1, null)) {|rows, i|
      rows[i] = rows[-(i + 1)] = wholeRow(A + i)
      return rows
    }
  }
}
