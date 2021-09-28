class QueenAttack {
  /* Instead of 2 constructors with a shared private method,
   * we have 1 constructor and a static method that passes
   * a default value.
   */
  static new() { this.new({}) }

  construct new(pieces) { 
    // If not specified, use their starting positions
    if (! pieces.containsKey("black")) pieces["black"] = [0,3]
    if (! pieces.containsKey("white")) pieces["white"] = [7,3]

    _blackPos = ChessPosition.new(pieces["black"])
    if (!_blackPos.valid) {
      Fiber.abort("Queen must be placed on the board")
    }

    _whitePos = ChessPosition.new(pieces["white"])
    if (!_whitePos.valid) {
      Fiber.abort("Queen must be placed on the board")
    }

    if (_whitePos == _blackPos) {
      Fiber.abort("Queens cannot share the same space")
    }
  }

  white { _whitePos.toList }
  black { _blackPos.toList }

  canAttack {
    var dx = (_whitePos.x - _blackPos.x).abs
    var dy = (_whitePos.y - _blackPos.y).abs
    return dx == 0 || dy == 0 || dx == dy
  }

  toString {
    var board = (0..7).map {|i| List.filled(8, "_")}.toList
    board[_whitePos.x][_whitePos.y] = "W"
    board[_blackPos.x][_blackPos.y] = "B"
    return board.map {|row| row.join(" ")}.join("\n")
  }
}

class ChessPosition {
  construct new(pos) {
    _x = pos[0]
    _y = pos[1]
  }

  x { _x }
  y { _y }
  toList { [x,y] }

  ==(other) { 
    return  Object.same(this, other) ||
            ( type == other.type &&
              x == other.x &&
              y == other.y )
  }

  valid { (0..7).contains(x) && (0..7).contains(y) }
}
