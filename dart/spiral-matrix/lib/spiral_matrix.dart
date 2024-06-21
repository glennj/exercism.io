typedef Matrix<T> = List<List<T>>;

class SpiralMatrix {
  final int size;
  SpiralMatrix(this.size);

  Matrix<int> _matrix = [];
  static const _empty = -1;

  Matrix<int> toList() {
    if (_matrix.length == 0) _generate();
    return _matrix;
  }

  void _generate() {
    _matrix = List.generate(size, (_) => List.filled(size, _empty));

    int x = 0, y = 0;
    int dx = 0, dy = 1;

    for (var i = 1; i <= size * size; i++) {
      _matrix[x][y] = i;

      if (!_validPos(x + dx, y + dy) || !_emptyPos(x + dx, y + dy)) {
        // turn the turtle
        [dx, dy] = [dy, -dx];
      }
      x += dx;
      y += dy;
    }
  }

  bool _validPos(x, y) => 0 <= x && x < size && 0 <= y && y < size;
  bool _emptyPos(x, y) => _matrix[x][y] == _empty;
}
