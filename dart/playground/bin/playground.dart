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

List<List<E>> permutations<E>(List<E> list) {
  switch (list.length) {
    case 0: return [];
    case 1: return [list];
    default:
      return list
        .fold([],
          (perms, elem) {
            var rest = list.where((e) => e != elem).toList();
            permutations(rest).forEach((perm) => perms.add([elem] + perm));
            return perms;
          }
        );
  }
}

/*
extension on List<E> {
  List<List<E>> permutations<E>() {
    switch (this.length) {
      case 0: return [];
      case 1: return [this];
      default:
        return fold(<E>[],
            (List<List<E>> perms, E elem) {
              var rest = this.where((E e) => e != elem).toList();
              for (List<E> perm in rest.permutations()) {
                perm.insert(0, elem);
                perms.add(perm);
              }
              return perms;
            }
          );
    }
  }
}
*/

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

  print(permutations([1,2,3]));
  print(permutations([1,2,3,4,5]));
  //print([1,2,3].permutations());
}
