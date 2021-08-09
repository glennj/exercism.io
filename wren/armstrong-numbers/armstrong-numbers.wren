// a more functional approach
class Number {
  static digits(n) {
    return n.toString.map {|char| Num.fromString(char)}
  }

  static isArmstrong(n) {
    var digits = digits(n)
    var len = digits.count
    return n == digits.reduce(0) {|sum, d| sum + d.pow(len)}
  }
}
