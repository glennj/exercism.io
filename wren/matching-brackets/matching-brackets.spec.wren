import "./matching-brackets" for Brackets
import "wren-testie/testie" for Testie, Expect

Testie.test("Matching Brackets") { |do, skip|
  do.test("paired square brackets") {
    Expect.value(Brackets.isPaired("[]")).toEqual(true)
  }

  do.test("empty string") {
    Expect.value(Brackets.isPaired("")).toEqual(true)
  }

  do.test("unpaired brackets") {
    Expect.value(Brackets.isPaired("[[")).toEqual(false)
  }

  do.test("wrong ordered brackets") {
    Expect.value(Brackets.isPaired("}{")).toEqual(false)
  }

  do.test("wrong closing bracket") {
    Expect.value(Brackets.isPaired("{]")).toEqual(false)
  }

  do.test("paired with whitespace") {
    Expect.value(Brackets.isPaired("{ }")).toEqual(true)
  }

  do.test("partially paired brackets") {
    Expect.value(Brackets.isPaired("{[])")).toEqual(false)
  }

  do.test("simple nested brackets") {
    Expect.value(Brackets.isPaired("{[]}")).toEqual(true)
  }

  do.test("several paired brackets") {
    Expect.value(Brackets.isPaired("{}[]")).toEqual(true)
  }

  do.test("paired and nested brackets") {
    Expect.value(Brackets.isPaired("([{}({}[])])")).toEqual(true)
  }

  do.test("unopened closing brackets") {
    Expect.value(Brackets.isPaired("{[)][]}")).toEqual(false)
  }

  do.test("unpaired and nested brackets") {
    Expect.value(Brackets.isPaired("([{])")).toEqual(false)
  }

  do.test("paired and wrong nested brackets") {
    Expect.value(Brackets.isPaired("[({]})")).toEqual(false)
  }

  do.test("paired and incomplete brackets") {
    Expect.value(Brackets.isPaired("{}[")).toEqual(false)
  }

  do.test("too many closing brackets") {
    Expect.value(Brackets.isPaired("[]]")).toEqual(false)
  }

  do.test("math expression") {
    Expect.value(Brackets.isPaired("(((185 + 223.85) * 15) - 543)/2")).toEqual(true)
  }

  do.test("complex latex expression") {
    Expect.value(
      Brackets.isPaired(
        "\\left(\\begin{array}{cc} \\frac{1}{3} & x\\\\ \\mathrm{e}^{x} &... x^2 \\end{array}\\right)"
      )
    ).toEqual(true)
  }
}
