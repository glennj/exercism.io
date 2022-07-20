import "./binary" for Binary
import "wren-testie/testie" for Testie, Expect

Testie.test("Binary") { |do, skip|
  do.test("0 is decimal 0") {
    Expect.value(Binary.new("0").toDecimal).toEqual(0)
  }

  do.test("1 is decimal 1") {
    Expect.value(Binary.new("1").toDecimal).toEqual(1)
  }

  do.test("10 is decimal 2") {
    Expect.value(Binary.new("10").toDecimal).toEqual(2)
  }

  do.test("11 is decimal 3") {
    Expect.value(Binary.new("11").toDecimal).toEqual(3)
  }

  do.test("100 is decimal 4") {
    Expect.value(Binary.new("100").toDecimal).toEqual(4)
  }

  do.test("1001 is decimal 9") {
    Expect.value(Binary.new("1001").toDecimal).toEqual(9)
  }

  do.test("11010 is decimal 26") {
    Expect.value(Binary.new("11010").toDecimal).toEqual(26)
  }

  do.test("10001101000 is decimal 1128") {
    Expect.value(Binary.new("10001101000").toDecimal).toEqual(1128)
  }

  do.test("ignores leading zeros") {
    Expect.value(Binary.new("00011111").toDecimal).toEqual(31)
  }

  do.test("invalid inputs are null") {
    // "2 is not a valid binary digit
    Expect.value(Binary.new("2").toDecimal).toEqual(null)

    // a number containing a non-binary digit is invalid
    Expect.value(Binary.new("01201").toDecimal).toEqual(null)

    // a number with trailing non-binary characters is invalid
    Expect.value(Binary.new("10nope").toDecimal).toEqual(null)

    // a number with leading non-binary characters is invalid
    Expect.value(Binary.new("nope10").toDecimal).toEqual(null)

    // a number with internal non-binary characters is invalid
    Expect.value(Binary.new("10nope10").toDecimal).toEqual(null)

    // a number and a word whitespace separated is invalid
    Expect.value(Binary.new("001nope").toDecimal).toEqual(null)
  }
}
