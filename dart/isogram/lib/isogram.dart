//final ALPHA = RegExp(r'\p{Letter}', caseSensitive: false);
final ALPHA = RegExp(r'[a-z]');

extension on String {
  bool isLetter() => ALPHA.hasMatch(this);
}

class Isogram {
  bool isIsogram(String word) {
    var seen = Set<String>();
    for (var char in word.toLowerCase().split('')) {
      if (char.isLetter()) {
        if (seen.contains(char)) return false;
        seen.add(char);
      }
    }
    return true;
  }
}
