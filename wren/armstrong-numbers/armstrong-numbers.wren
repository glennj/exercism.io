// This iteration does not need to convert from num to string back to num
class Number {
  static digitLength(n) {
    var len = (n.log / 10.log).ceil
    if (n == 10.pow(len)) len = len + 1
    return len
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
    return sum == input
  }
}
