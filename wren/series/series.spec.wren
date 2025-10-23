import "./series" for Series
import "wren-testie/testie" for Testie, Expect

Testie.test("Series") { |do, skip|
  do.test("slices of one from one") {
    var expected = [
      "1",
    ]
    Expect.value(Series.new("1").slices(1)).toEqual(expected)
  }

  do.test("slices of one from two") {
    var expected = [
      "1",
      "2",
    ]
    Expect.value(Series.new("12").slices(1)).toEqual(expected)
  }

  do.test("slices of two") {
    var expected = [
      "35",
    ]
    Expect.value(Series.new("35").slices(2)).toEqual(expected)
  }

  do.test("slices of two overlap") {
    var expected = [
      "91",
      "14",
      "42",
    ]
    Expect.value(Series.new("9142").slices(2)).toEqual(expected)
  }

  do.test("slices can include duplicates") {
    var expected = [
      "777",
      "777",
      "777",
      "777",
    ]
    Expect.value(Series.new("777777").slices(3)).toEqual(expected)
  }

  do.test("slices of a long series") {
    var expected = [
      "91849",
      "18493",
      "84939",
      "49390",
      "93904",
      "39042",
      "90424",
      "04243",
    ]
    Expect.value(Series.new("918493904243").slices(5)).toEqual(expected)
  }

  do.test("slice length is too large") {
    Expect.that {
      Series.new("12345").slices(6)
    }.abortsWith("slice length cannot be greater than series length")
  }

  do.test("slice length is way too large") {
    Expect.that {
      Series.new("12345").slices(42)
    }.abortsWith("slice length cannot be greater than series length")
  }

  do.test("slice length cannot be zero") {
    Expect.that {
      Series.new("12345").slices(0)
    }.abortsWith("slice length cannot be zero")
  }

  do.test("slice length cannot be negative") {
    Expect.that {
      Series.new("123").slices(-1)
    }.abortsWith("slice length cannot be negative")
  }

  do.test("empty series is invalid") {
    Expect.that {
      Series.new("").slices(1)
    }.abortsWith("series cannot be empty")
  }
}
