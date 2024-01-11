import 'package:playground/playground.dart' as playground;

class Factorial {
  var _fs = <int>[1, 1, 2, 6, 24];

  int factorial(int n) {
    for (var i = _fs.length; i <= n; i++) {
      print(i);
      _fs.add(i * _fs.last);
    }
    return _fs[n];
  }
}

void main(List<String> arguments) {
  print('Hello world: ${playground.calculate()}!');

  for (var re in [
                  RegExp('[a-z]'),
                  RegExp(r'\p{Alphabetic}', unicode: true),
                  RegExp(r'\p{Decimal_Number}', unicode: true),
                ]) {
    print(re);
    var str = "Hello";
    print([str, re.hasMatch(str)]);
    str = "1234";
    print([str, re.hasMatch(str)]);
  }

  var f = Factorial();
  print(f.factorial(6));
  print(f.factorial(5));
}
