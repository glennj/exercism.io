class PhoneNumber {
  static clean(input) {
    // remove valid non-digits
    var cleaned = [" ", "(", ")", "-", ".", "+"].reduce(input) {|s, c| s.replace(c, "")}

    if (cleaned.count < 10) Fiber.abort("must not be fewer than 10 digits")
    if (cleaned.count > 11) Fiber.abort("must not be greater than 11 digits")
    if (cleaned.count == 11) {
      if (!cleaned.startsWith("1")) Fiber.abort("11 digits must start with 1")
      cleaned = cleaned[1..-1]
    }

    var cps = cleaned.codePoints
    if (cps.any {|cp|  isAlpha(cp)}) Fiber.abort("letters not permitted")
    if (cps.any {|cp| !isDigit(cp)}) Fiber.abort("punctuations not permitted")

    if (cleaned[0] == "0") Fiber.abort("area code cannot start with zero")
    if (cleaned[0] == "1") Fiber.abort("area code cannot start with one")
    if (cleaned[3] == "0") Fiber.abort("exchange code cannot start with zero")
    if (cleaned[3] == "1") Fiber.abort("exchange code cannot start with one")

    return cleaned
  }

  static isDigit(codePoint) { 48 <= codePoint && codePoint <= 57 }
  static isAlpha(codePoint) { isLower(codePoint) || isUpper(codePoint)}
  static isUpper(codePoint) { 65 <= codePoint && codePoint <= 90 }
  static isLower(codePoint) { 97 <= codePoint && codePoint <= 122 }
}
