import "./minesweeper" for Minesweeper
import "wren-testie/testie" for Testie, Expect

Testie.test("Minesweeper") { |do, skip|
  do.test("handles no rows") {
    Expect.value(Minesweeper.annotate([])).toEqual([])
  }

  skip.test("handles no columns") {
    Expect.value(Minesweeper.annotate([""])).toEqual([""])
  }

  skip.test("handles no mines") {
    var input = ["   ", "   ", "   "]
    var expected = ["   ", "   ", "   "]
    Expect.value(Minesweeper.annotate(input)).toEqual(expected)
  }

  skip.test("handles minefield with only mines") {
    var input = ["***", "***", "***"]
    var expected = ["***", "***", "***"]
    Expect.value(Minesweeper.annotate(input)).toEqual(expected)
  }

  skip.test("handles mine surrounded by spaces") {
    var input = ["   ", " * ", "   "]
    var expected = ["111", "1*1", "111"]
    Expect.value(Minesweeper.annotate(input)).toEqual(expected)
  }

  skip.test("handles space surrounded by mines") {
    var input = ["***", "* *", "***"]
    var expected = ["***", "*8*", "***"]
    Expect.value(Minesweeper.annotate(input)).toEqual(expected)
  }

  skip.test("handles horizontal line") {
    var input = [" * * "]
    var expected = ["1*2*1"]
    Expect.value(Minesweeper.annotate(input)).toEqual(expected)
  }

  skip.test("handles horizontal line, mines at edges") {
    var input = ["*   *"]
    var expected = ["*1 1*"]
    Expect.value(Minesweeper.annotate(input)).toEqual(expected)
  }

  skip.test("handles vertical line") {
    var input = [" ", "*", " ", "*", " "]
    var expected = ["1", "*", "2", "*", "1"]
    Expect.value(Minesweeper.annotate(input)).toEqual(expected)
  }

  skip.test("handles vertical line, mines at edges") {
    var input = ["*", " ", " ", " ", "*"]
    var expected = ["*", "1", " ", "1", "*"]
    Expect.value(Minesweeper.annotate(input)).toEqual(expected)
  }

  skip.test("handles cross") {
    var input = ["  *  ", "  *  ", "*****", "  *  ", "  *  "]
    var expected = [" 2*2 ", "25*52", "*****", "25*52", " 2*2 "]
    Expect.value(Minesweeper.annotate(input)).toEqual(expected)
  }

  skip.test("handles large minefield") {
    var input = [" *  * ", "  *   ", "    * ", "   * *", " *  * ", "      "]
    var expected = [
      "1*22*1",
      "12*322",
      " 123*2",
      "112*4*",
      "1*22*2",
      "111111",
    ]
    Expect.value(Minesweeper.annotate(input)).toEqual(expected)
  }
}
