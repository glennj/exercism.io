class FlowerField {
  final List<String> field;
  FlowerField(this.field);

  final _annotated = <String>[];

  List<String> get annotated {
    if (_annotated.isEmpty) _annotate();
    return _annotated;
  }

  void _annotate() {
    var annotated = field
        .map((row) => row.split('').map((c) => c == '*' ? 99 : 0).toList())
        .toList();

    bool onBoard(int x, int y) =>
        (0 <= y && y < field.length) && (0 <= x && x < field[y].length);

    void incrNeighbours(int x, int y) {
      for (var dy in [-1, 0, 1]) {
        var yy = y + dy;
        for (var dx in [-1, 0, 1]) {
          var xx = x + dx;
          if (onBoard(xx, yy) && annotated[yy][xx] != 99) {
            annotated[yy][xx]++;
          }
        }
      }
    }

    for (var y = 0; y < annotated.length; y++) {
      for (var x = 0; x < annotated[y].length; x++) {
        if (annotated[y][x] == 99) incrNeighbours(x, y);
      }
    }

    for (var row in annotated) {
      var str = row
          .map((n) => switch (n) { 0 => ' ', 99 => '*', _ => n.toString() })
          .join('');
      _annotated.add(str);
    }
  }
}
