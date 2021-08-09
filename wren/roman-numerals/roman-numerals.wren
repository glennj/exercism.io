// A Map would have been nice for this structure, but
// it does not preserve order.
var DECIMAL_TO_ROMAN = [
  [1000, "M"], [900, "CM"], [500, "D"], [400, "CD"],
  [ 100, "C"], [ 90, "XC"], [ 50, "L"], [ 40, "XL"],
  [  10, "X"], [  9, "IX"], [  5, "V"], [  4, "IV"],
  [   1, "I"],
]

class Number {
  static toRoman(value) {
    return DECIMAL_TO_ROMAN.reduce("") {|roman, pair|
      while (value >= pair[0]) {
        roman = roman + pair[1]
        value = value - pair[0]
      }
      return roman
    }
  }
}
