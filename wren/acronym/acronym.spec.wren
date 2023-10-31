import "./acronym" for Acronym
import "wren-testie/testie" for Testie, Expect

Testie.test("Acronyms") { |do, skip|
  // basic
  do.test("title cased phrases") {
    Expect.value(Acronym.parse("Portable Network Graphics")).toEqual("PNG")
  }

  // lowercase words
  do.test("other title cased phrases") {
    Expect.value(Acronym.parse("Ruby on Rails")).toEqual("ROR")
  }

  // punctuation
  do.test("phrases with punctuation") {
    Expect.value(Acronym.parse("First In, First Out")).toEqual("FIFO")
  }

  // all caps word
  do.test("phrases with all uppercase words") {
    Expect.value(Acronym.parse("GNU Image Manipulation Program")).toEqual("GIMP")
  }

  // punctuation without whitespace
  do.test("phrases with punctuation without whitespace") {
    Expect.value(Acronym.parse("Complementary metal-oxide semiconductor")).toEqual("CMOS")
  }

  // very long abbreviation
  do.test("long phrases") {
    Expect.value(
      Acronym.parse(
        "Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me"
      )
    ).toEqual("ROTFLSHTMDCOALM")
  }

  // consecutive delimiters
  do.test("phrases with consecutive delimiters") {
    Expect.value(Acronym.parse("Something - I made up from thin air")).toEqual("SIMUFTA")
  }

  // apostrophes
  do.test("phrases with apostrophes") {
    Expect.value(Acronym.parse("Halley's Comet")).toEqual("HC")
  }

  // underscore emphasis
  do.test("phrases with underscore emphasis") {
    Expect.value(Acronym.parse("The Road _Not_ Taken")).toEqual("TRNT")
  }
}
