import "./ocr-numbers" for OcrNumbers
import "wren-testie/testie" for Testie, Expect

Testie.test("OcrNumbers") { |do, skip|
  do.test("Recognizes 0") {
    var rows = [
      " _ ",
      "| |",
      "|_|",
      "   ",
    ]
    Expect.value(OcrNumbers.convert(rows)).toEqual("0")
  }

  do.test("Recognizes 1") {
    var rows = [
      "   ",
      "  |",
      "  |",
      "   ",
    ]
    Expect.value(OcrNumbers.convert(rows)).toEqual("1")
  }

  do.test("Unreadable but correctly sized inputs return ?") {
    var rows = [
      "   ",
      "  _",
      "  |",
      "   ",
    ]
    Expect.value(OcrNumbers.convert(rows)).toEqual("?")
  }

  do.test("Input with a number of lines that is not a multiple of four raises an error") {
    var rows = [
      " _ ",
      "| |",
      "   ",
    ]
    Expect.that {
      OcrNumbers.convert(rows)
    }.abortsWith("Number of input lines is not a multiple of four")
  }

  do.test("Input with a number of columns that is not a multiple of three raises an error") {
    var rows = [
      "    ",
      "   |",
      "   |",
      "    ",
    ]
    Expect.that {
      OcrNumbers.convert(rows)
    }.abortsWith("Number of input columns is not a multiple of three")
  }

  do.test("Recognizes 110101100") {
    var rows = [
      "       _     _        _  _ ",
      "  |  || |  || |  |  || || |",
      "  |  ||_|  ||_|  |  ||_||_|",
      "                           ",
    ]
    Expect.value(OcrNumbers.convert(rows)).toEqual("110101100")
  }

  do.test("Garbled numbers in a string are replaced with ?") {
    var rows = [
      "       _     _           _ ",
      "  |  || |  || |     || || |",
      "  |  | _|  ||_|  |  ||_||_|",
      "                           ",
    ]
    Expect.value(OcrNumbers.convert(rows)).toEqual("11?10?1?0")
  }

  do.test("Recognizes 2") {
    var rows = [
      " _ ",
      " _|",
      "|_ ",
      "   ",
    ]
    Expect.value(OcrNumbers.convert(rows)).toEqual("2")
  }

  do.test("Recognizes 3") {
    var rows = [
      " _ ",
      " _|",
      " _|",
      "   ",
    ]
    Expect.value(OcrNumbers.convert(rows)).toEqual("3")
  }

  do.test("Recognizes 4") {
    var rows = [
      "   ",
      "|_|",
      "  |",
      "   ",
    ]
    Expect.value(OcrNumbers.convert(rows)).toEqual("4")
  }

  do.test("Recognizes 5") {
    var rows = [
      " _ ",
      "|_ ",
      " _|",
      "   ",
    ]
    Expect.value(OcrNumbers.convert(rows)).toEqual("5")
  }

  do.test("Recognizes 6") {
    var rows = [
      " _ ",
      "|_ ",
      "|_|",
      "   ",
    ]
    Expect.value(OcrNumbers.convert(rows)).toEqual("6")
  }

  do.test("Recognizes 7") {
    var rows = [
      " _ ",
      "  |",
      "  |",
      "   ",
    ]
    Expect.value(OcrNumbers.convert(rows)).toEqual("7")
  }

  do.test("Recognizes 8") {
    var rows = [
      " _ ",
      "|_|",
      "|_|",
      "   ",
    ]
    Expect.value(OcrNumbers.convert(rows)).toEqual("8")
  }

  do.test("Recognizes 9") {
    var rows = [
      " _ ",
      "|_|",
      " _|",
      "   ",
    ]
    Expect.value(OcrNumbers.convert(rows)).toEqual("9")
  }

  do.test("Recognizes string of decimal numbers") {
    var rows = [
      "    _  _     _  _  _  _  _  _ ",
      "  | _| _||_||_ |_   ||_||_|| |",
      "  ||_  _|  | _||_|  ||_| _||_|",
      "                              ",
    ]
    Expect.value(OcrNumbers.convert(rows)).toEqual("1234567890")
  }

  do.test("Numbers separated by empty lines are recognized. Lines are joined by commas.") {
    var rows = [
      "    _  _ ",
      "  | _| _|",
      "  ||_  _|",
      "         ",
      "    _  _ ",
      "|_||_ |_ ",
      "  | _||_|",
      "         ",
      " _  _  _ ",
      "  ||_||_|",
      "  ||_| _|",
      "         ",
    ]
    Expect.value(OcrNumbers.convert(rows)).toEqual("123,456,789")
  }
}
