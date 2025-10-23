import "./assert" for Assert
import "./byte" for Byte

class PhoneNumber {
  static clean(input) {
    // remove valid non-digits
    var cleaned = [" ", "(", ")", "-", ".", "+"].reduce(input) {|s, c| s.replace(c, "")}

    Assert.refute(cleaned.count < 10, "must not be fewer than 10 digits")
    Assert.refute(cleaned.count > 11, "must not be greater than 11 digits")

    if (cleaned.count == 11) {
      Assert.assert(cleaned[0] == "1", "11 digits must start with 1")
      cleaned = cleaned[1..-1]
    }

    var bytes = Byte.bytes(cleaned)
    Assert.refute(bytes.any {|b| b.isAlpha}, "letters not permitted")
    Assert.assert(bytes.all {|b| b.isDigit}, "punctuations not permitted")

    Assert.assert(cleaned[0] != "0", "area code cannot start with zero")
    Assert.assert(cleaned[0] != "1", "area code cannot start with one")
    Assert.assert(cleaned[3] != "0", "exchange code cannot start with zero")
    Assert.assert(cleaned[3] != "1", "exchange code cannot start with one")

    return cleaned
  }
}
