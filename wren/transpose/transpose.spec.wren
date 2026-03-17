import "./transpose" for Transpose
import "wren-testie/testie" for Testie, Expect

Testie.test("Transpose") { |do, skip|
  do.test("empty string") {
    var lines = []
    var expected = []
    Expect.value(Transpose.new(lines).transpose).toEqual(expected)
  }

  do.test("two characters in a row") {
    var lines = [
      "A1",
    ]
    var expected = [
      "A",
      "1",
    ]
    Expect.value(Transpose.new(lines).transpose).toEqual(expected)
  }

  do.test("two characters in a column") {
    var lines = [
      "A",
      "1",
    ]
    var expected = [
      "A1",
    ]
    Expect.value(Transpose.new(lines).transpose).toEqual(expected)
  }

  do.test("simple") {
    var lines = [
      "ABC",
      "123",
    ]
    var expected = [
      "A1",
      "B2",
      "C3",
    ]
    Expect.value(Transpose.new(lines).transpose).toEqual(expected)
  }

  do.test("single line") {
    var lines = [
      "Single line.",
    ]
    var expected = [
      "S",
      "i",
      "n",
      "g",
      "l",
      "e",
      " ",
      "l",
      "i",
      "n",
      "e",
      ".",
    ]
    Expect.value(Transpose.new(lines).transpose).toEqual(expected)
  }

  do.test("first line longer than second line") {
    var lines = [
      "The fourth line.",
      "The fifth line.",
    ]
    var expected = [
      "TT",
      "hh",
      "ee",
      "  ",
      "ff",
      "oi",
      "uf",
      "rt",
      "th",
      "h ",
      " l",
      "li",
      "in",
      "ne",
      "e.",
      ".",
    ]
    Expect.value(Transpose.new(lines).transpose).toEqual(expected)
  }

  do.test("second line longer than first line") {
    var lines = [
      "The first line.",
      "The second line.",
    ]
    var expected = [
      "TT",
      "hh",
      "ee",
      "  ",
      "fs",
      "ie",
      "rc",
      "so",
      "tn",
      " d",
      "l ",
      "il",
      "ni",
      "en",
      ".e",
      " .",
    ]
    Expect.value(Transpose.new(lines).transpose).toEqual(expected)
  }

  do.test("mixed line length") {
    var lines = [
      "The longest line.",
      "A long line.",
      "A longer line.",
      "A line.",
    ]
    var expected = [
      "TAAA",
      "h   ",
      "elll",
      " ooi",
      "lnnn",
      "ogge",
      "n e.",
      "glr",
      "ei ",
      "snl",
      "tei",
      " .n",
      "l e",
      "i .",
      "n",
      "e",
      ".",
    ]
    Expect.value(Transpose.new(lines).transpose).toEqual(expected)
  }

  do.test("square") {
    var lines = [
      "HEART",
      "EMBER",
      "ABUSE",
      "RESIN",
      "TREND",
    ]
    var expected = [
      "HEART",
      "EMBER",
      "ABUSE",
      "RESIN",
      "TREND",
    ]
    Expect.value(Transpose.new(lines).transpose).toEqual(expected)
  }

  do.test("rectangle") {
    var lines = [
      "FRACTURE",
      "OUTLINED",
      "BLOOMING",
      "SEPTETTE",
    ]
    var expected = [
      "FOBS",
      "RULE",
      "ATOP",
      "CLOT",
      "TIME",
      "UNIT",
      "RENT",
      "EDGE",
    ]
    Expect.value(Transpose.new(lines).transpose).toEqual(expected)
  }

  do.test("triangle") {
    var lines = [
      "T",
      "EE",
      "AAA",
      "SSSS",
      "EEEEE",
      "RRRRRR",
    ]
    var expected = [
      "TEASER",
      " EASER",
      "  ASER",
      "   SER",
      "    ER",
      "     R",
    ]
    Expect.value(Transpose.new(lines).transpose).toEqual(expected)
  }

  do.test("jagged triangle") {
    var lines = [
      "11",
      "2",
      "3333",
      "444",
      "555555",
      "66666",
    ]
    var expected = [
      "123456",
      "1 3456",
      "  3456",
      "  3 56",
      "    56",
      "    5",
    ]
    Expect.value(Transpose.new(lines).transpose).toEqual(expected)
  }
}
