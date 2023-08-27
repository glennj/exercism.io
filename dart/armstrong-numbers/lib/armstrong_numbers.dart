import 'dart:math';

class ArmstrongNumbers {
  bool isArmstrongNumber(String number) {
    var num = int.parse(number);
    var sum = 0;

    if (num > 0) {
      // for fun, get the length with math instead of string length
      var len = (log(num) / ln10).ceil();

      while (num > 0) {
        var digit = num % 10;
        sum += digit ^ len;
        num ~/= 10;
      }
    }
    return sum == int.parse(number);
  }
}
