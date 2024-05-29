class RotationalCipher {
  static var upper = RegExp(r'[A-Z]');
  static var lower = RegExp(r'[a-z]');
  static const code_a = 97;
  static const code_A = 65;
  static const alphabet_size = 26;

  String rotate({required String text, required int shiftKey}) {
    return text
        .replaceAllMapped(upper, (m) => shift(m[0]!, shiftKey, code_A))
        .replaceAllMapped(lower, (m) => shift(m[0]!, shiftKey, code_a));
  }

  String shift(String letter, int key, int base) {
    var ascii = letter.codeUnitAt(0);
    var shifted = (((ascii - base) + key) % alphabet_size) + base;
    return String.fromCharCode(shifted);
  }
}
