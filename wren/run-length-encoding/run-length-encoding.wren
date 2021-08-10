class RLE {

  /* Encoding "AABCCCDEEEE":
   * 1) turn it into:    [["A",2],["B",1],["C",3],["D",1],["E",4]]
   * 2) remove 1 counts: [["A",2],["B",""],["C",3],["D",""],["E",4]]
   * 3) map that into:   ["2A", "B", "3C", "D", "4E"]
   * 4) join into:       "2AB3CD4E"
   */
  static encode(s) {
    var prev = ""
    return s
      .reduce([]) {|runs, char|
        if (prev != char) {
          prev = char
          runs.add([char, 1])
        } else {
          runs[-1] = [runs[-1][0], runs[-1][1] + 1]
        }
        return runs
      }
      .map {|run| run[1] == 1 ? [run[0], ""] : run}
      .map {|run| "%(run[1])%(run[0])"}
      .join()
  }

  static decode(s) {
    var count = 0
    return s
      .reduce([]) {|pieces, char|
        if (isDigit(char)) {
          count = count * 10 + Num.fromString(char)
        } else {
          pieces.add(strRepeat(char, 1.max(count)))
          count = 0
        }
        return pieces
      }
      .join()
  }

  static isDigit(char) { "0123456789".contains(char) }

  static strRepeat(char, count) { List.filled(count, char).join() }
}
