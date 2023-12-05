class RotationalCipher {
  static rotate(text, shiftKey) {

    var rotateLetter = Fn.new {|codePoint, offset|
      return (codePoint - offset + shiftKey) % 26 + offset
    }

    var rotate = Fn.new {|codePoint|
      if (65 <= codePoint && codePoint <=  90) return rotateLetter.call(codePoint, 65)
      if (97 <= codePoint && codePoint <= 122) return rotateLetter.call(codePoint, 97)
      return codePoint
    }

    return text.codePoints.map(rotate).map {|cp| String.fromCodePoint(cp)}.join()
  }
}
