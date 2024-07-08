import "./secret-handshake" for SecretHandshake
import "wren-testie/testie" for Testie, Expect

Testie.test("SecretHandshake") { |do, skip|
  do.test("wink for 1") {
    var actual = SecretHandshake.commands(1)
    var expected = ["wink"]
    Expect.value(actual).toEqual(expected)
  }

  skip.test("double blink for 10") {
    var actual = SecretHandshake.commands(2)
    var expected = ["double blink"]
    Expect.value(actual).toEqual(expected)
  }

  skip.test("close your eyes for 100") {
    var actual = SecretHandshake.commands(4)
    var expected = ["close your eyes"]
    Expect.value(actual).toEqual(expected)
  }

  skip.test("jump for 1000") {
    var actual = SecretHandshake.commands(8)
    var expected = ["jump"]
    Expect.value(actual).toEqual(expected)
  }

  skip.test("combine two actions") {
    var actual = SecretHandshake.commands(3)
    var expected = ["wink", "double blink"]
    Expect.value(actual).toEqual(expected)
  }

  skip.test("reverse two actions") {
    var actual = SecretHandshake.commands(19)
    var expected = ["double blink", "wink"]
    Expect.value(actual).toEqual(expected)
  }

  skip.test("reversing one action gives the same action") {
    var actual = SecretHandshake.commands(24)
    var expected = ["jump"]
    Expect.value(actual).toEqual(expected)
  }

  skip.test("reversing no actions still gives no actions") {
    var actual = SecretHandshake.commands(16)
    var expected = []
    Expect.value(actual).toEqual(expected)
  }

  skip.test("all possible actions") {
    var actual = SecretHandshake.commands(15)
    var expected = ["wink", "double blink", "close your eyes", "jump"]
    Expect.value(actual).toEqual(expected)
  }

  skip.test("reverse all possible actions") {
    var actual = SecretHandshake.commands(31)
    var expected = ["jump", "close your eyes", "double blink", "wink"]
    Expect.value(actual).toEqual(expected)
  }

  skip.test("do nothing for zero") {
    var actual = SecretHandshake.commands(0)
    var expected = []
    Expect.value(actual).toEqual(expected)
  }
}
