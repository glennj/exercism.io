var Tiny = [
  "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
  "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
  "seventeen", "eighteen", "nineteen"
]
var Tens = [
  null, null, "twenty", "thirty", "forty", "fifty", "sixty",
  "seventy", "eighty", "ninety"
]

class Say {
  static say(number) {
    if (number < 0) {
      Fiber.abort("input out of range")
    }
    if (number < 100) {
      return saySmall(number)
    }
    if (number < 1000) {
      return sayCompound(number, 100, "hundred")
    }
    if (number < 1e6) {
      return sayCompound(number, 1e3, "thousand")
    }
    if (number < 1e9) {
      return sayCompound(number, 1e6, "million")
    }
    if (number < 1e12) {
      return sayCompound(number, 1e9, "billion")
    }
    Fiber.abort("input out of range")
  }

  static saySmall(number) {
    if (number < 20) {
      return Tiny[number]
    }
    var tens = (number / 10).floor
    var ones = number % 10
    if (ones == 0) {
      return Tens[tens]
    } else {
      return "%(Tens[tens])-%(Tiny[ones])"
    }
  }

  static sayCompound(number, base, unit) {
    var first = (number / base).floor
    var rest = number % base
    if (rest == 0) {
      return "%(say(first)) %(unit)"
    } else {
      return "%(say(first)) %(unit) %(say(rest))"
    }
  }
}
