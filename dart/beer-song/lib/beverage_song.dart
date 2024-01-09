extension on String {
  // from perl: https://perldoc.pl/functions/ucfirst
  String ucFirst() {
    if (length < 2)
      return toUpperCase();
    else
      return this[0].toUpperCase() + substring(1);
  }
}

class BeverageSong {
  static const MAX = 99;

  // constructor allows stuff like:
  //    final juiceSong = BeverageSong(liquid: 'juice', where: 'in the fridge');

  final String liquid;
  final String where;
  BeverageSong({required this.liquid, required this.where});

  List<String> recite(int startVerse, int numVerses) {
    return List
        .generate(numVerses, (i) => startVerse - i)
        .fold(<String>[], (vs, n) => vs..addAll(['', _first(n), _second(n)]))
        .sublist(1);
  }

  String _first(int n) {
    var b = _bottles(n);
    return '${b.ucFirst()} $where, $b.';
  }

  String _second(int n) {
    var b = '${_bottles(n - 1)} $where.';
    if (n > 0)
      return 'Take ${n == 1 ? "it" : "one"} down and pass it around, $b';
    else
      return 'Go to the store and buy some more, $b';
  }

  String _bottles(int n) {
    if (n < 0) n = MAX;
    return '${n == 0 ? "no more" : n} bottle${n == 1 ? "" : "s"} of $liquid';
  }
}
