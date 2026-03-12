import "./say" for Say
import "wren-testie/testie" for Testie, Expect

Testie.test("Say") { |do, skip|
  do.test("zero") {
    var actual = Say.say(0)
    var expected = "zero"
    Expect.value(actual).toEqual(expected)
  }

  do.test("one") {
    var actual = Say.say(1)
    var expected = "one"
    Expect.value(actual).toEqual(expected)
  }

  do.test("fourteen") {
    var actual = Say.say(14)
    var expected = "fourteen"
    Expect.value(actual).toEqual(expected)
  }

  do.test("twenty") {
    var actual = Say.say(20)
    var expected = "twenty"
    Expect.value(actual).toEqual(expected)
  }

  do.test("twenty-two") {
    var actual = Say.say(22)
    var expected = "twenty-two"
    Expect.value(actual).toEqual(expected)
  }

  do.test("thirty") {
    var actual = Say.say(30)
    var expected = "thirty"
    Expect.value(actual).toEqual(expected)
  }

  do.test("ninety-nine") {
    var actual = Say.say(99)
    var expected = "ninety-nine"
    Expect.value(actual).toEqual(expected)
  }

  do.test("one hundred") {
    var actual = Say.say(100)
    var expected = "one hundred"
    Expect.value(actual).toEqual(expected)
  }

  do.test("one hundred twenty-three") {
    var actual = Say.say(123)
    var expected = "one hundred twenty-three"
    Expect.value(actual).toEqual(expected)
  }

  do.test("two hundred") {
    var actual = Say.say(200)
    var expected = "two hundred"
    Expect.value(actual).toEqual(expected)
  }

  do.test("nine hundred ninety-nine") {
    var actual = Say.say(999)
    var expected = "nine hundred ninety-nine"
    Expect.value(actual).toEqual(expected)
  }

  do.test("one thousand") {
    var actual = Say.say(1000)
    var expected = "one thousand"
    Expect.value(actual).toEqual(expected)
  }

  do.test("one thousand two hundred thirty-four") {
    var actual = Say.say(1234)
    var expected = "one thousand two hundred thirty-four"
    Expect.value(actual).toEqual(expected)
  }

  do.test("one million") {
    var actual = Say.say(1000000)
    var expected = "one million"
    Expect.value(actual).toEqual(expected)
  }

  do.test("one million two thousand three hundred forty-five") {
    var actual = Say.say(1002345)
    var expected = "one million two thousand three hundred forty-five"
    Expect.value(actual).toEqual(expected)
  }

  do.test("one billion") {
    var actual = Say.say(1000000000)
    var expected = "one billion"
    Expect.value(actual).toEqual(expected)
  }

  do.test("a big number") {
    var actual = Say.say(987654321123)
    var expected = "nine hundred eighty-seven billion six hundred fifty-four million three hundred twenty-one thousand one hundred twenty-three"
    Expect.value(actual).toEqual(expected)
  }

  do.test("numbers below zero are out of range") {
    Expect.that {
      Say.say(-1)
    }.abortsWith("input out of range")
  }

  do.test("numbers above 999,999,999,999 are out of range") {
    Expect.that {
      Say.say(1000000000000)
    }.abortsWith("input out of range")
  }
}
