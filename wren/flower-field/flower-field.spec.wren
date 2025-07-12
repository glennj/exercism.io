import "./flower-field" for FlowerField
import "wren-testie/testie" for Testie, Expect

Testie.test("FlowerField") { |do, skip|
  do.test("handles no rows") {
    Expect.value(FlowerField.annotate([])).toEqual([])
  }

  skip.test("handles no columns") {
    Expect.value(FlowerField.annotate([""])).toEqual([""])
  }

  skip.test("handles no flowers") {
    var input = [
      "   ",
      "   ",
      "   "
    ]
    var expected = [
      "   ",
      "   ",
      "   "
    ]
    Expect.value(FlowerField.annotate(input)).toEqual(expected)
  }

  skip.test("handles garden with only flowers") {
    var input = [
      "***",
      "***",
      "***"
    ]
    var expected = [
      "***",
      "***",
      "***"
    ]
    Expect.value(FlowerField.annotate(input)).toEqual(expected)
  }

  skip.test("handles flower surrounded by spaces") {
    var input = [
      "   ",
      " * ",
      "   "
    ]
    var expected = [
      "111",
      "1*1",
      "111"
    ]
    Expect.value(FlowerField.annotate(input)).toEqual(expected)
  }

  skip.test("handles space surrounded by flowers") {
    var input = [
      "***",
      "* *",
      "***"
    ]
    var expected = [
      "***",
      "*8*",
      "***"
    ]
    Expect.value(FlowerField.annotate(input)).toEqual(expected)
  }

  skip.test("handles horizontal line") {
    var input = [
      " * * "
    ]
    var expected = [
      "1*2*1"
    ]
    Expect.value(FlowerField.annotate(input)).toEqual(expected)
  }

  skip.test("handles horizontal line, flowers at edges") {
    var input = [
      "*   *"
    ]
    var expected = [
      "*1 1*"
    ]
    Expect.value(FlowerField.annotate(input)).toEqual(expected)
  }

  skip.test("handles vertical line") {
    var input = [
      " ",
      "*",
      " ",
      "*",
      " "
    ]
    var expected = [
      "1",
      "*",
      "2",
      "*",
      "1"
    ]
    Expect.value(FlowerField.annotate(input)).toEqual(expected)
  }

  skip.test("handles vertical line, flowers at edges") {
    var input = [
      "*",
      " ",
      " ",
      " ",
      "*"
    ]
    var expected = [
      "*",
      "1",
      " ",
      "1",
      "*"
    ]
    Expect.value(FlowerField.annotate(input)).toEqual(expected)
  }

  skip.test("handles cross") {
    var input = [
      "  *  ",
      "  *  ",
      "*****",
      "  *  ",
      "  *  "
    ]
    var expected = [
      " 2*2 ",
      "25*52",
      "*****",
      "25*52",
      " 2*2 "
    ]
    Expect.value(FlowerField.annotate(input)).toEqual(expected)
  }

  skip.test("handles large garden") {
    var input = [
      " *  * ",
      "  *   ",
      "    * ",
      "   * *",
      " *  * ",
      "      "
    ]
    var expected = [
      "1*22*1",
      "12*322",
      " 123*2",
      "112*4*",
      "1*22*2",
      "111111",
    ]
    Expect.value(FlowerField.annotate(input)).toEqual(expected)
  }
}
