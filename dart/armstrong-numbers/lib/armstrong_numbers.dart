class ArmstrongNumbers {
  bool isArmstrongNumber(String number) => BigInt.parse(number) == armstrongSum(number);

  BigInt armstrongSum(String number) =>
      number.runes
            .where((rune) => 48 <= rune && rune <= 57)   // ascii digits
            .map((rune) => BigInt.from(rune - 48))       // convert to numbers
            .fold(BigInt.zero, (sum, digit) => sum + digit.pow(number.length));
}
