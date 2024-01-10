final ALPHA = RegExp(r'\p{Alphabetic}', caseSensitive: false, unicode: true);

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
