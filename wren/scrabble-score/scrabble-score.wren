import "./byte" for Byte

class Scrabble {
  static score(word) {
    return word.reduce(0) {|sum, letter|
      return sum + ScrabbleTile.new(letter).value
    }
  }
}

class ScrabbleTile {
  construct new(letter) {
    _letter = Byte.upcase(letter)
    _value = __scores.containsKey(_letter) ? __scores[_letter] : 0
  }
  value  { _value }
  letter { _letter }

  // instantiate class fields, if necessary
  static initialize() {
    if (__scores == null) {
      __scores = {
        "A": 1, "E": 1, "I": 1, "O": 1, "U": 1,
        "L": 1, "N": 1, "R": 1, "S": 1, "T": 1,
        "D": 2, "G": 2,
        "B": 3, "C": 3, "M": 3, "P": 3,
        "F": 4, "H": 4, "V": 4, "W": 4, "Y": 4,
        "K": 5,
        "J": 8, "X": 8,
        "Q": 10, "Z": 10,
      }
    }
  }
}
ScrabbleTile.initialize()
