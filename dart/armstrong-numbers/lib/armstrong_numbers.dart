class ArmstrongNumbers {
  bool isArmstrongNumber(String number) {
    var len = number.length;
    var sum = number.runes
                    .where((rune) => 48 <= rune && rune <= 57)
                    .map((rune) => BigInt.from(rune - 48))
                    .fold(BigInt.from(0), (sum, digit) => sum + digit.pow(len));
    return sum.toString() == number;
  }
}
