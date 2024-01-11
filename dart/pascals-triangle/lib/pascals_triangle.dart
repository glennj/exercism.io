class PascalsTriangle {
  final fact = Factorial();

  List<List<int>> rows(int n) =>
      List.generate(n, (i) => List.generate(i + 1, (j) => fact.choose(i, j)));
}

/* a memoizing factorial calculator */
class Factorial {
  final _fs = <int>[1, 1, 2, 6, 24]; // seed it with a few factorials

  int factorial(int n) {
    for (var i = _fs.length; i <= n; i++) {
      _fs.add(i * _fs.last);
    }
    return _fs[n];
  }

  int choose(int n, int k) => factorial(n) ~/ factorial(k) ~/ factorial(n - k);
}
