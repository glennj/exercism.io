class CollatzConjecture {
  int _next(int n) => n.isEven ? (n ~/ 2) : (3 * n + 1);

  int steps(int n, [int moves = 0]) {
    if (n <= 0) throw ArgumentError('Only positive integers are allowed');
    if (n == 1) return moves;
    return steps(_next(n), moves + 1);
  }
}
