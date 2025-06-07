typedef Matrix = List<List<int>>;

class GameOfLife {
  Matrix _mtx;

  GameOfLife(this._mtx);

  // TODO: return a clone or a frozen version.
  Matrix matrix() => _mtx;

  Matrix tick() {
    Matrix next = _newMatrix();

    for (var row = 0; row < _mtx.length; row++) {
      for (var col = 0; col < _mtx[row].length; col++) {
        next[row][col] = switch (_countNeighbours(row, col)) {
          3 => 1,               // becomes live
          2 => _mtx[row][col],  // stays live if live
          _ => 0,               // dies
        };
      }
    }

    _mtx = next;
    return next;
  }

  Matrix _newMatrix() =>
      List.generate(_mtx.length, (i) => List.filled(_mtx[i].length, 0));

  int _countNeighbours(int row, int col) {
    var count = 0;

    for (var delta in [[-1,-1], [-1,0], [-1,1], [0,-1], [0,1], [1,-1], [1,0], [1,1]]) {
      var dr = row + delta[0];
      var dc = col + delta[1];
      if (0 <= dr && dr < _mtx.length &&
          0 <= dc && dc < _mtx[row].length) { count += _mtx[dr][dc]; }
    }

    return count;
  }
}
