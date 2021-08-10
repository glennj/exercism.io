var SPACE = " ".bytes[0]
var MINE = "*".bytes[0]
var ZERO = "0".bytes[0]

class Minesweeper {
  static annotate(grid) { this.new(grid).annotate().toString }

  construct new(grid) {
    if (grid.isEmpty) {
      _board = []
      _rows = _cols = 0
    } else {
      _board = grid.map {|row| row.bytes.toList}.toList
      _rows = _board.count
      _cols = _board[0].count
    }
  }

  toString {
    return _board
      .map {|row| row.map {|b| String.fromByte(b)}.join()}
      .toList
  }

  annotate() {
    (0..._rows).each {|row|
      (0..._cols).each {|col|
        if (_board[row][col] == SPACE) {
          var n = numNeighbouringMines(row, col)
          if (n > 0) {
            _board[row][col] = ZERO + n
          }
        }
      }
    }
    return this
  }

  numNeighbouringMines(r, c) {
    return (-1..1).map {|dr| r + dr}
      .where {|rr| (0..._rows).contains(rr)}
      .reduce(0) {|mines, rr|
        return mines +
          (-1..1).map {|dc| c + dc}
            .where {|cc| (0..._cols).contains(cc)}
            .where {|cc| _board[rr][cc] == MINE}
            .count
      }
  }
}
