class PhoneNumber {
  String clean(String input) {
    // remove valid non-digits
    var cleaned =
        input.replaceAll(RegExp(r'\s'), '').replaceAll(RegExp(r'[+().-]'), '');

    if (cleaned.length < 10) {
      throw FormatException('must not be fewer than 10 digits');
    }
    if (cleaned.length > 11) {
      throw FormatException('must not be greater than 11 digits');
    }
    if (cleaned.length == 11) {
      if (!cleaned.startsWith('1')) {
        throw FormatException('11 digits must start with 1');
      }
      cleaned = cleaned.substring(1);
    }

    if (cleaned.contains(RegExp(r'\p{Alpha}', unicode: true))) {
      throw FormatException('letters not permitted');
    }
    // we've already checked letters: any non-digits are "punctuation"
    if (RegExp(r'\D').hasMatch(cleaned)) {
      throw FormatException('punctuations not permitted');
    }

    if (cleaned[0] == '0') {
      throw FormatException('area code cannot start with zero');
    }
    if (cleaned[0] == '1') {
      throw FormatException('area code cannot start with one');
    }
    if (cleaned[3] == '0') {
      throw FormatException('exchange code cannot start with zero');
    }
    if (cleaned[3] == '1') {
      throw FormatException('exchange code cannot start with one');
    }

    return cleaned;
  }
}
