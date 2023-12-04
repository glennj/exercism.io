import "./byte" for Byte

class BottleSong {
  static recite(startBottles, takeDown) {
    return BottleSong.new(startBottles, takeDown).verses
  }

  construct new(startBottles, takeDown) {
    _from = startBottles
    _howMany = takeDown
  }

  static initialize {
    __NUMBERS = ["No", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten"]
  }

  verses {
    var lines = []
    var separate = false
    for (i in _from..(_from - _howMany + 1)) {
      if (separate) lines.add("")
      lines.addAll(verse(i))
      separate = true
    }
    return lines
  }

  verse(n) {
    var v = []
    v.add("%(bottle(n)) hanging on the wall,")
    v.add(v[0])
    v.add("And if one green bottle should accidentally fall,")
    v.add("There'll be %(Byte.downcase(bottle(n-1))) hanging on the wall.")
    return v
  }

  bottle(n) { 
    return "%(__NUMBERS[n]) green bottle%(n == 1 ? "" : "s")"
  }
}

BottleSong.initialize
