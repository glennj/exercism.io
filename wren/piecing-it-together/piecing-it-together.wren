class PiecingItTogether {
  static jigsawData(partial) { PiecingItTogether.new(partial).info }

  construct new(partial) {
    _input = partial
  }

  // with the rows and cols, all else falls into place
  data(rows, cols) {
    var border = 2 * (rows + cols - 2)
    var ratio = cols / rows
    return {
      "rows": rows,
      "columns": cols,
      "pieces": rows * cols,
      "border": border,
      "inside": rows * cols - border,
      "aspectRatio": ratio,
      "format": (ratio > 1) ? "landscape" : (ratio < 1 ? "portrait" : "square")
    }
  }

  meetsCriteria(jigsaw) {
    for (key in jigsaw.keys) {
      if (_input.containsKey(key) && _input[key] != jigsaw[key]) {
        return false
      }
    }
    return true
  }

  maxDim { 1000 } // in the absense of other info, a maximum dimension

  info {
    var possibilities = []
    var i = 0
    if (_input.containsKey("pieces")) {
      for (r in 1.._input["pieces"]) {
        if (_input["pieces"] % r == 0) {
          var jigsaw = data(r, _input["pieces"] / r)
          if (meetsCriteria(jigsaw)) {
            possibilities.add(jigsaw)
          }
        }
      }
    } else if (_input.containsKey("inside")) {
      for (r in 1.._input["inside"]) {
        if (_input["inside"] % r == 0) {
          var jigsaw = data(2 + r, 2 + _input["inside"] / r)
          if (meetsCriteria(jigsaw)) {
            possibilities.add(jigsaw)
          }
        }
      }
    } else {
      // brute force, check all possibilities in a big grid
      for (r in maxDim..1) {
        for (c in maxDim..1) {
          var jigsaw = data(r, c)
          if (meetsCriteria(jigsaw)) {
            possibilities.add(jigsaw)
          }
        }
      }
    }

    if (possibilities.count == 0) Fiber.abort("Contradictory data")
    if (possibilities.count  > 1) Fiber.abort("Insufficient data")
    return possibilities[0]
  }
}
