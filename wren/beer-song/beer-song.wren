// With extreme efforts to minimize duplication of hardcoded strings.

class BeerSong {
  static recite(verseNum, numVerses) {
    return (0...numVerses).reduce([]) {|lines, i| 
      if (numVerses > 1 && i > 0) lines.add("")
      lines.addAll(verse(verseNum - i))
      return lines
    }
  }

  static verse(n) {
    var where = "on the wall"
    return [
      "%(bottles(n, true)) %(where), %(bottles(n)).",
      "%(action(n)), %(bottles(next(n))) %(where)."
    ]
  }

  static bottles(n) { bottles(n, false) }
  static bottles(n, capitalize) {
    return "%(howMany(n, capitalize)) bottle%(plural(n)) of beer"
  }
  static next(n) { n == 0 ? 99 : n-1 }
  static howMany(n, up) { n == 0 ? (up?"N":"n")+"o more" : n }
  static plural(n) { n == 1 ? "" : "s" }
  static action(n) {
    return n == 0 ?
      "Go to the store and buy some more" :
      "Take %(one(n)) down and pass it around"
  }
  static one(n) { n == 1 ? "it" : "one" }
}
