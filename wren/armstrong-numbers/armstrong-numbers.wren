// This iteration does not need to convert from num to string back to num
class Number {
  static digitLength(n) {
    var exp = n.log / 10.log
    return exp == exp.ceil ? exp + 1 : exp.ceil
  }

  static isArmstrong(n) {
    var sum = 0
    var len = digitLength(n)
    var input = n
    while (n > 0) {
      var digit = n % 10
      sum = sum + digit.pow(len)
      n = (n - digit) / 10
    }
    System.print([input, sum])
    return sum == input
  }
}
