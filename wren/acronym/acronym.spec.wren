import "./acronym" for Acronym
import "wren-testie/testie" for Testie, Expect

Testie.test("Acronyms") { |do, skip|
  // basic
  do.test("title cased phrases") {
    Expect.value(Acronym.parse("Portable Network Graphics")).toEqual("PNG")
  }

  // lowercase words
  skip.test("other title cased phrases") {
    Expect.value(Acronym.parse("Ruby on Rails")).toEqual("ROR")
  }

  // punctuation
  skip.test("phrases with punctuation") {
    Expect.value(Acronym.parse("First In, First Out")).toEqual("FIFO")
  }

  // all caps word
  skip.test("phrases with all uppercase words") {
    Expect.value(Acronym.parse("GNU Image Manipulation Program")).toEqual("GIMP")
  }

  // punctuation without whitespace
  skip.test("phrases with punctuation without whitespace") {
    Expect.value(Acronym.parse("Complementary metal-oxide semiconductor")).toEqual("CMOS")
  }

  // very long abbreviation
  skip.test("long phrases") {
    Expect.value(
      Acronym.parse(
        "Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me"
      )
    ).toEqual("ROTFLSHTMDCOALM")
  }

  // consecutive delimiters
  skip.test("phrases with consecutive delimiters") {
    Expect.value(Acronym.parse("Something - I made up from thin air")).toEqual("SIMUFTA")
  }

  // apostrophes
  skip.test("phrases with apostrophes") {
    Expect.value(Acronym.parse("Halley's Comet")).toEqual("HC")
  }

  // underscore emphasis
  skip.test("phrases with underscore emphasis") {
    Expect.value(Acronym.parse("The Road _Not_ Taken")).toEqual("TRNT")
  }
}
