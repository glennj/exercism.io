import "./largest-series-product" for Series
import "wren-testie/testie" for Testie, Expect

Testie.test("LargestSeriesProduct") { |do, skip|

  do.test("finds the largest product if span equals length") {
    var actual = Series.new("29", 2).largestProduct
    var expected = 18
    Expect.value(actual).toEqual(expected)
  }

  skip.test("can find the largest product of 2 with numbers in order") {
    var actual = Series.new("0123456789", 2).largestProduct
    var expected = 72
    Expect.value(actual).toEqual(expected)
  }

  skip.test("can find the largest product of 2") {
    var actual = Series.new("576802143", 2).largestProduct
    var expected = 48
    Expect.value(actual).toEqual(expected)
  }

  skip.test("can find the largest product of 3 with numbers in order") {
    var actual = Series.new("0123456789", 3).largestProduct
    var expected = 504
    Expect.value(actual).toEqual(expected)
  }

  skip.test("can find the largest product of 3") {
    var actual = Series.new("1027839564", 3).largestProduct
    var expected = 270
    Expect.value(actual).toEqual(expected)
  }

  skip.test("can find the largest product of 5 with numbers in order") {
    var actual = Series.new("0123456789", 5).largestProduct
    var expected = 15120
    Expect.value(actual).toEqual(expected)
  }

  skip.test("can get the largest product of a big number") {
    var actual = Series.new("73167176531330624919225119674426574742355349194934", 6).largestProduct
    var expected = 23520
    Expect.value(actual).toEqual(expected)
  }

  skip.test("reports zero if the only digits are zero") {
    var actual = Series.new("0000", 2).largestProduct
    var expected = 0
    Expect.value(actual).toEqual(expected)
  }

  skip.test("reports zero if all spans include zero") {
    var actual = Series.new("99099", 3).largestProduct
    var expected = 0
    Expect.value(actual).toEqual(expected)
  }

  skip.test("rejects span longer than string length") {
    Expect.that {
      Series.new("123", 4).largestProduct
    }.abortsWith("span must not exceed string length")
  }

  skip.test("reports 1 for empty string and empty product (0 span)") {
    var actual = Series.new("", 0).largestProduct
    var expected = 1
    Expect.value(actual).toEqual(expected)
  }

  skip.test("reports 1 for nonempty string and empty product (0 span)") {
    var actual = Series.new("123", 0).largestProduct
    var expected = 1
    Expect.value(actual).toEqual(expected)
  }

  skip.test("rejects empty string and nonzero span") {
    Expect.that {
      Series.new("", 1).largestProduct
    }.abortsWith("span must not exceed string length")
  }

  skip.test("rejects invalid character in digits") {
    Expect.that {
      Series.new("1234a5", 2).largestProduct
    }.abortsWith("digits input must only contain digits")
  }

  skip.test("rejects negative span") {
    Expect.that {
      Series.new("12345", -1).largestProduct
    }.abortsWith("span must not be negative")
  }
}
