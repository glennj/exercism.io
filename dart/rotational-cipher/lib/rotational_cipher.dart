class RotationalCipher {
  static var upper = RegExp(r'[A-Z]');
  static var lower = RegExp(r'[a-z]');

  int shiftKey = 0;

  String rotate({required String text, required int shiftKey}) {
    this.shiftKey = shiftKey;
    return text
        .replaceAllMapped(upper, rotateUpper)
        .replaceAllMapped(lower, rotateLower);
  }

  String rotateUpper(Match m) => rotateLetter(m[0]!, 65); // 'A'
  String rotateLower(Match m) => rotateLetter(m[0]!, 97); // 'a'

  String rotateLetter(String letter, int base) =>
      String.fromCharCode(((letter.codeUnitAt(0) - base) + shiftKey) % 26 + base);
}
