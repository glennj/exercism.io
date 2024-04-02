extension ConvertToRomanNumerals on int {
  String toRoman() {
    String convert(int dec, String rom) {
      String addDigit(int base, List<String> digits) {
        if (dec >= 10 * base) return convert(dec - 10 * base, rom + digits[0]);
        if (dec >= 9 * base) return convert(dec + 1 * base, rom + digits[2]);
        if (dec >= 5 * base) return convert(dec - 5 * base, rom + digits[1]);
        if (dec >= 4 * base) return convert(dec + 1 * base, rom + digits[2]);
        return convert(dec - 1 * base, rom + digits[2]);
      }

      if (dec > 399) return addDigit(100, ['M', 'D', 'C']);
      if (dec > 39) return addDigit(10, ['C', 'L', 'X']);
      if (dec > 0) return addDigit(1, ['X', 'V', 'I']);
      return rom;
    }

    if (this < 0) throw ArgumentError('non-negative integers only');
    return convert(this, "");
  }
}
