// Extensions for the Num class
class NumExt {

  // return the ordinal of a number: 1st, 2nd, 3rd, 4th, etc
  static nth(number) {
    var suffix = Fn.new {|n|
      var ones = n % 10
      var tens = n % 100
      if (ones == 1 && tens != 11) return "st"
      if (ones == 2 && tens != 12) return "nd"
      if (ones == 3 && tens != 13) return "rd"
      return "th"
    }
    return number.toString + suffix.call(number)
  }
}
