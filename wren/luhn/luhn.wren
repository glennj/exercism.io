/* This is every 2nd digit index is "doubled"
 * idx 0: val = key * 2 > 10 ? key * 2 - 9 : key * 2
 * idx 1: val = key
 */
var DigitValue = [
  {0:0, 1:2, 2:4, 3:6, 4:8, 5:1, 6:3, 7:5, 8:7, 9:9},
  {0:0, 1:1, 2:2, 3:3, 4:4, 5:5, 6:6, 7:7, 8:8, 9:9},
]

class Luhn {
  static valid(number) {
    number = number.replace(" ", "")
    if (number.count < 2) return false
    if (number.any {|c| !"0123456789".contains(c)}) return false

    var idx = 0
    var sum = number[-1..0]
      .map {|char| Num.fromString(char)}
      .reduce(0) {|sum, digit| sum + DigitValue[(idx = idx + 1) % 2][digit]}

    return sum % 10 == 0
  }

}
