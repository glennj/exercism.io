import 'range.dart';

typedef IntFn = int Function(int x);

class DifferenceOfSquares {
  int _ident(int n) => n;
  int _square(int n) => n * n;

  int squareOfSum(int n) => _doSum(n, _ident, _square);
  int sumOfSquares(int n) => _doSum(n, _square, _ident);

  int _doSum(int n, IntFn innerFn, IntFn outerFn) {
    int sum = 1.upto(n).fold(0, (s, i) => s + innerFn(i));
    return outerFn(sum);
  }

  int differenceOfSquares(int n) => (sumOfSquares(n) - squareOfSum(n)).abs();
}
