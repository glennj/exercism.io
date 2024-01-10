import 'package:playground/playground.dart' as playground;

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
}
