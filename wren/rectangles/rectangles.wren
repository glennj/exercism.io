class Rectangles {
  construct new(lines) {
    _lines = lines[0..-1]
    _vertices = Vertices.from(_lines)
    _count = null
  }

  count {
    if (_count == null) {
      _count = 0

      for (topLeft in _vertices) {
        var rightOf = _vertices.rightOf(topLeft)
        var below = _vertices.below(topLeft)

        for (topRight in rightOf) {
          for (bottomLeft in below) {
            var bottomRight = Coord.new(bottomLeft.row, topRight.col)

            if (_vertices.contains(bottomRight)) {
              if (isRectangle_(topLeft, bottomRight)) {
                _count = _count + 1
              }
            }
          }
        }
      }
    }
    return _count
  }

  isRectangle_(topLeft, bottomRight) {
    return isHorizontalLine_(topLeft.row, topLeft.col, bottomRight.col) &&
           isHorizontalLine_(bottomRight.row, topLeft.col, bottomRight.col) &&
           isVerticalLine_(topLeft.col, topLeft.row, bottomRight.row) &&
           isVerticalLine_(bottomRight.col, topLeft.row, bottomRight.row)
  }

  isHorizontalLine_(row, col1, col2) {
    return (col1..col2).map {|c| _lines[row][c]}
                       .all {|ch| "+-".contains(ch)}
  }
  isVerticalLine_(col, row1, row2) {
    return (row1..row2).map {|r| _lines[r][col]}
                       .all {|ch| "+|".contains(ch)}
  }
}

/* ************************************************** */
class Vertices {
  static from(lines) { new().collectFrom_(lines) }

  construct new() {
    _vertices = []
  }

  collectFrom_(lines) {
    for (r in 0...lines.count) {
      for (c in 0...lines[r].count) {
        if (lines[r][c] == "+") {
          _vertices.add(Coord.new(r, c))
        }
      }
    }
    return this
  }

  // these methods return WhereSequences that can be iterated
  rightOf(vertex) { _vertices.where {|v| v.row == vertex.row && v.col > vertex.col} }
  below(vertex)   { _vertices.where {|v| v.col == vertex.col && v.row > vertex.row} }

  // delegating to the list
  contains(vertex) { _vertices.contains(vertex) }
  // iterator protocol
  iterate(iterator) { _vertices.iterate(iterator) }
  iteratorValue(iterator) { _vertices.iteratorValue(iterator) }
}

/* ************************************************** */
class Coord {
  construct new(row, col) {
    _row = row
    _col = col
  }
  row {_row}
  col {_col}

  // needed for `_vertices.contains(vertex)`
  ==(other) {type == other.type && row == other.row && col == other.col}
}
