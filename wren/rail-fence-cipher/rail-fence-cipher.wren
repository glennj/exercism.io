class RailFenceCipher {
  static encode(msg, rails) { this.new(msg, rails).encipher("E") }
  static decode(msg, rails) { this.new(msg, rails).encipher("D") }

  construct new(msg, rails) {
    _msg = msg
    _rails = rails
  }

  encipher(type) {
    var indices = map_indices()
    var chars = _msg.codePoints.map {|cp| String.fromCodePoint(cp)}.toList
    var result = List.filled(_msg.count, "")
    for (i in 0...chars.count) {
      if (type == "E") {
        result[i] = chars[indices[i]]   // encoding
      } else {
        result[indices[i]] = chars[i]   // decoding
      }
    }
    return result.join("")
  }

  map_indices() {
    var next_rail = Fiber.new {
      var rail = 0
      var direction = 1
      while (true) {
        Fiber.yield(rail)
        if ((rail == 0 && direction == -1) || (rail == _rails - 1 && direction == 1)) {
          direction = -direction
        }
        rail = rail + direction
      }
    }

    var rails = []
    for (i in 1.._rails) {
      rails.add([])
    }

    for (i in 0..._msg.count) {
      var idx = next_rail.call()
      rails[idx].add(i)
    }

    return rails.reduce([]) {|acc, rail|
      acc.addAll(rail)
      return acc
    }
  }
}
