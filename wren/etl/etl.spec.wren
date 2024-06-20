import "./etl" for Etl
import "wren-testie/testie" for Testie, Expect

Testie.test("ETL") { |do, skip|
  do.test("single letter") {
    var legacy = { 1: ["A"] }
    var actual = Etl.transform(legacy)
    var expected = { "a": 1 }
    Expect.value(actual).toEqual(expected)
  }

  do.test("single score with multiple letters") {
    var legacy = { 1: ["A", "E", "I", "O", "U"] }
    var actual = Etl.transform(legacy)
    var expected = { "a": 1, "e": 1, "i": 1, "o": 1, "u": 1 }
    Expect.value(actual).toEqual(expected)
  }

  do.test("multiple scores with multiple letters") {
    var legacy = { 1: ["A", "E"], 2: ["D", "G"] }
    var actual = Etl.transform(legacy)
    var expected = { "a": 1, "d": 2, "e": 1, "g": 2 }
    Expect.value(actual).toEqual(expected)
  }

  do.test("multiple scores with differing numbers of letters") {
    var legacy = {
      1: ["A", "E", "I", "O", "U", "L", "N", "R", "S", "T"],
      2: ["D", "G"],
      3: ["B", "C", "M", "P"],
      4: ["F", "H", "V", "W", "Y"],
      5: ["K"],
      8: ["J", "X"],
      10: ["Q", "Z"]
    }
    var actual = Etl.transform(legacy)
    var expected = {
      "a": 1,   "b": 3, "c": 3, "d": 2, "e": 1, "f": 4, "g": 2, "h": 4,
      "i": 1,   "j": 8, "k": 5, "l": 1, "m": 3, "n": 1, "o": 1, "p": 3,
      "q": 10,  "r": 1, "s": 1, "t": 1, "u": 1, "v": 4, "w": 4, "x": 8,
      "y": 4,   "z": 10
    }
    Expect.value(actual).toEqual(expected)
  }
}

