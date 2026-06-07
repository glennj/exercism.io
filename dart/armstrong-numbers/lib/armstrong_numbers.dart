class ArmstrongNumbers {
  bool isArmstrongNumber(String number) {
    var n = BigInt.parse(number);
    var len = number.length;
    var sum = number.runes
                    .where((rune) => 48 <= rune && rune <= 57)   // ascii digits
                    .map((rune) => BigInt.from(rune - 48))       // convert to numbers
                    .fold(BigInt.zero, (sum, digit) => sum + digit.pow(len));
    
    return n == sum;
  }
}
