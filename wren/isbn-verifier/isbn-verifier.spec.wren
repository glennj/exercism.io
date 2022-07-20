import "./isbn-verifier" for ISBN
import "wren-testie/testie" for Testie, Expect

Testie.test("ISBN Verifier") { |do, skip|
  do.test("valid isbn") {
    Expect.value(ISBN.new("3-598-21508-8").isValid).toEqual(true)
  }

  do.test("invalid isbn check digit") {
    Expect.value(ISBN.new("3-598-21508-9").isValid).toEqual(false)
  }

  do.test("valid isbn number with a check digit of 10") {
    Expect.value(ISBN.new("3-598-21507-X").isValid).toEqual(true)
  }

  do.test("check digit is a character other than X") {
    Expect.value(ISBN.new("3-598-21507-A").isValid).toEqual(false)
  }

  do.test("invalid character in isbn") {
    Expect.value(ISBN.new("3-598-P1581-X").isValid).toEqual(false)
  }

  do.test("X is only valid as a check digit") {
    Expect.value(ISBN.new("3-598-2X507-9").isValid).toEqual(false)
  }

  do.test("valid isbn without separating dashes") {
    Expect.value(ISBN.new("3598215088").isValid).toEqual(true)
  }

  do.test("isbn without separating dashes and X as check digit") {
    Expect.value(ISBN.new("359821507X").isValid).toEqual(true)
  }

  do.test("isbn without check digit and dashes") {
    Expect.value(ISBN.new("359821507").isValid).toEqual(false)
  }

  do.test("too long isbn and no dashes") {
    Expect.value(ISBN.new("3598215078X").isValid).toEqual(false)
  }

  do.test("too short isbn") {
    Expect.value(ISBN.new("00").isValid).toEqual(false)
  }

  do.test("isbn without check digit") {
    Expect.value(ISBN.new("3-598-21507").isValid).toEqual(false)
  }

  do.test("check digit of X should not be used for 0") {
    Expect.value(ISBN.new("3-598-21515-X").isValid).toEqual(false)
  }

  do.test("empty isbn") {
    Expect.value(ISBN.new("").isValid).toEqual(false)
  }

  do.test("input is 9 characters") {
    Expect.value(ISBN.new("134456729").isValid).toEqual(false)
  }

  do.test("invalid characters are not ignored") {
    Expect.value(ISBN.new("3132P34035").isValid).toEqual(false)
  }

  do.test("input is too long but contains a valid isbn") {
    Expect.value(ISBN.new("98245726788").isValid).toEqual(false)
  }
}
