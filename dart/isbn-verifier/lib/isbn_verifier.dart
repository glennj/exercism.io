bool isValid(String input) {
  var cleaned = input.replaceAll('-', '');

  if (!RegExp(r'^\d{9}[\dX]$').hasMatch(cleaned)) return false;

  var digits =
      cleaned.split('').map((c) => c == 'X' ? 10 : int.parse(c)).toList();

  var sum = List.generate(digits.length, (i) => digits[i] * (10 - i))
      .reduce((sum, val) => sum + val);

  return sum % 11 == 0;
}
