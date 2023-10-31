import "wren-testie/testie" for Testie, Expect
import "./isogram" for Isogram

Testie.test("Isogram") { |do, skip|
  do.test("empty string") {
    Expect.value(Isogram.isIsogram("")).toBe(true)
  }

  do.test("isogram with only lower case characters") {
    Expect.value(Isogram.isIsogram("isogram")).toBe(true)
  }

  do.test("word with one duplicated character") {
    Expect.value(Isogram.isIsogram("eleven")).toBe(false)
  }

  do.test("word with one duplicated character from the end of the alphabet") {
    Expect.value(Isogram.isIsogram("zzyzx")).toBe(false)
  }

  do.test("longest reported english isogram") {
    Expect.value(Isogram.isIsogram("subdermatoglyphic")).toBe(true)
  }

  do.test("word with duplicated character in mixed case") {
    Expect.value(Isogram.isIsogram("Alphabet")).toBe(false)
  }

  do.test("word with duplicated character in mixed case, lowercase first") {
    Expect.value(Isogram.isIsogram("alphAbet")).toBe(false)
  }

  do.test("hypothetical isogrammic word with hyphen") {
    Expect.value(Isogram.isIsogram("thumbscrew-japingly")).toBe(true)
  }

  do.test("hypothetical word with duplicated character following hyphen") {
    Expect.value(Isogram.isIsogram("thumbscrew-jappingly")).toBe(false)
  }

  do.test("isogram with duplicated hyphen") {
    Expect.value(Isogram.isIsogram("six-year-old")).toBe(true)
  }

  do.test("made-up name that is an isogram") {
    Expect.value(Isogram.isIsogram("Emily Jung Schwartzkopf")).toBe(true)
  }

  do.test("duplicated character in the middle") {
    Expect.value(Isogram.isIsogram("accentor")).toBe(false)
  }

  do.test("same first and last characters") {
    Expect.value(Isogram.isIsogram("angola")).toBe(false)
  }

  do.test("word with duplicated character and with two hyphens") {
    Expect.value(Isogram.isIsogram("up-to-date")).toBe(false)
  }
}
