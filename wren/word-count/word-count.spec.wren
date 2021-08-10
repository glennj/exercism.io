import "./word-count" for Words
import "wren-testie/testie" for Testie, Expect

Testie.test("Words") { |do, skip|
  do.test("count one word") {
    var expectedCounts = { "word": 1 }
    Expect.value(Words.count("word")).toEqual(expectedCounts)
  }

  do.test("count one of each word") {
    var expectedCounts = { "one": 1, "of": 1, "each": 1 }
    Expect.value(Words.count("one of each")).toEqual(expectedCounts)
  }

  do.test("multiple occurrences of a word") {
    var expectedCounts = {
      "one": 1,
      "fish": 4,
      "two": 1,
      "red": 1,
      "blue": 1,
    }
    Expect.value(Words.count("one fish two fish red fish blue fish")).toEqual(
      expectedCounts
    )
  }

  do.test("handles cramped lists") {
    var expectedCounts = {
      "one": 1,
      "two": 1,
      "three": 1,
    }
    Expect.value(Words.count("one,two,three")).toEqual(expectedCounts)
  }

  do.test("handles expanded lists") {
    var expectedCounts = {
      "one": 1,
      "two": 1,
      "three": 1,
    }
    Expect.value(Words.count("one,\ntwo,\nthree")).toEqual(expectedCounts)
  }

  do.test("ignore punctuation") {
    var expectedCounts = {
      "car": 1,
      "carpet": 1,
      "as": 1,
      "java": 1,
      "javascript": 1,
    }
    Expect.value(Words.count("car: carpet as java: javascript!!&@$\%^&")).toEqual(
      expectedCounts
    )
  }

  do.test("include numbers") {
    var expectedCounts = {
      "testing": 2,
      "1": 1,
      "2": 1,
      "33": 1
    }
    Expect.value(Words.count("testing, 1, 2, 33 testing")).toEqual(expectedCounts)
  }

  do.test("normalize case") {
    var expectedCounts = {
      "go": 3,
      "stop": 2,
    }
    Expect.value(Words.count("go Go GO Stop stop")).toEqual(expectedCounts)
  }

  do.test("with apostrophes") {
    var expectedCounts = {
      "first": 1,
      "don't": 2,
      "laugh": 1,
      "then": 1,
      "cry": 1,
    }
    Expect.value(Words.count("First: don't laugh. Then: don't cry.")).toEqual(
      expectedCounts
    )
  }

  do.test("with quotations") {
    var expectedCounts = {
      "joe": 1,
      "can't": 1,
      "tell": 1,
      "between": 1,
      "large": 2,
      "and": 1,
    }
    Expect.value(Words.count("Joe can't tell between 'large' and large.")).toEqual(
      expectedCounts
    )
  }

  do.test("substrings from the beginning") {
    var expectedCounts = {
      "joe": 1,
      "can't": 1,
      "tell": 1,
      "between": 1,
      "app": 1,
      "apple": 1,
      "and": 1,
      "a": 1,
    }
    Expect.value(Words.count("Joe can't tell between app, apple and a.")).toEqual(
      expectedCounts
    )
  }

  do.test("multiple spaces not detected as a word") {
    var expectedCounts = {
      "multiple": 1,
      "whitespaces": 1,
    }
    Expect.value(Words.count(" multiple   whitespaces")).toEqual(expectedCounts)
  }

  do.test("alternating word separators not detected as a word") {
    var expectedCounts = {
      "one": 1,
      "two": 1,
      "three": 1
    }
    Expect.value(Words.count(",\n,one,\n ,two \n 'three'")).toEqual(expectedCounts)
  }
}
