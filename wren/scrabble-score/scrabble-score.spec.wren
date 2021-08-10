import "wren-testie/testie" for Testie, Expect
import "./scrabble-score" for Scrabble

Testie.test("Scrabble") { |do, skip|
  do.test("lowercase letter") {
    Expect.value(Scrabble.score("a")).toEqual(1)
  }

  do.test("uppercase letter") {
    Expect.value(Scrabble.score("A")).toEqual(1)
  }

  do.test("valuable letter") {
    Expect.value(Scrabble.score("f")).toEqual(4)
  }

  do.test("short word") {
    Expect.value(Scrabble.score("at")).toEqual(2)
  }

  do.test("short, valuable word") {
    Expect.value(Scrabble.score("zoo")).toEqual(12)
  }

  do.test("medium word") {
    Expect.value(Scrabble.score("street")).toEqual(6)
  }

  do.test("medium, valuable word") {
    Expect.value(Scrabble.score("quirky")).toEqual(22)
  }

  do.test("long, mixed-case word") {
    Expect.value(Scrabble.score("OxyphenButazone")).toEqual(41)
  }

  do.test("english-like word") {
    Expect.value(Scrabble.score("pinata")).toEqual(8)
  }

  do.test("empty input") {
    Expect.value(Scrabble.score("")).toEqual(0)
  }

  do.test("entire alphabet available") {
    Expect.value(Scrabble.score("abcdefghijklmnopqrstuvwxyz")).toEqual(87)
  }
}
