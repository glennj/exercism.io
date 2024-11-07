class StateOfTicTacToe {
  static gamestate(board) { StateOfTicTacToe.new(board).state }

  construct new(board) {
    _board = board.join("")
    if (_board.count != 9) Fiber.abort("Invalid board")

    _X = (0...9).where {|i| _board[i] == "X"}.toList
    _O = (0...9).where {|i| _board[i] == "O"}.toList
  }

  state {
    if (_X.count - _O.count > 1) Fiber.abort("Wrong turn order: X went twice")
    if (_X.count - _O.count < 0) Fiber.abort("Wrong turn order: O started")

    var xWin = isWin(_X)
    var oWin = isWin(_O)
    if (xWin && oWin) Fiber.abort("Impossible board: game should have ended after the game was won")
    if (xWin || oWin) return "win"

    return (_X.count + _O.count == 9) ? "draw" : "ongoing"
  }

  isWin(positions) {
    var winningMasks = [
        7,    // 111 000 000
       56,    // 000 111 000
      448,    // 000 000 111
       73,    // 100 100 100
      146,    // 010 010 010
      292,    // 001 001 001
       84,    // 001 010 100
      273,    // 100 010 001
    ]
    var value = positions.reduce(0) {|val, i| val | (1 << i)}
    for (mask in winningMasks) {
      if (value & mask == mask) return true
    }
    return false
  }
}
