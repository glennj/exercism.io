class GameOfLife {
  static tick(matrix) {
    var game = GameOfLife.new(matrix)
    game.tick()
    return game.matrix
  }

  construct new(matrix) {
    _mx = matrix.map {|row| row[0..-1]}.toList
  }

  matrix { _mx }

  tick() {
    _mx = (0..._mx.count).map {|i| 
            return (0..._mx[i].count).map {|j|
              var n = countNeighbours(i, j)
              if (n == 3) return 1
              if (n == 2) return _mx[i][j]
              return 0
            }.toList
          }.toList
  }

  countNeighbours(i, j) {
    return [[-1,-1], [-1,0], [-1,1], [0,-1], [0,1], [1,-1], [1,0], [1,1]] .
      where {|d| 0 <= i+d[0] && i+d[0] < _mx.count &&
                 0 <= j+d[1] && j+d[1] < _mx[i].count } .
      reduce(0) {|sum, d| sum + _mx[i + d[0]][j + d[1]]}
  }
}
