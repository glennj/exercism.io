import "./string-utils" for StringUtils as Str

class Transpose {
  construct new(lines) {
    _lines = lines
  }

  transpose {
    if (_lines.isEmpty) return []

    var maxWidth = _lines.map {|line| line.count}.reduce {|max, size| max.max(size)}
    var transposed = (0...maxWidth).map {List.filled(_lines.count, " ")}.toList
    for (r in 0..._lines.count) {
      for (c in 0..._lines[r].count) {
        transposed[c][r] = _lines[r][c]
      }
    }

    // from the bottom to the top, each line is at least as long as the one below it.
    var width = 0
    for (i in (maxWidth - 1)..0) {
      var line = transposed[i].join("").trimEnd()
      width = width.max(line.count)
      transposed[i] = Str.padEnd(line, width)
    }
    return transposed
  }
}
