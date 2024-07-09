import "./anagram" for Anagram
import "wren-testie/testie" for Testie, Expect

Testie.test("Anagram") { |do, skip|
  do.test("no matches") {
    Expect.value(
      Anagram.find("diaper", ["hello", "world", "zombies", "pants"])
    ).toEqual([])
  }

  do.test("detects two anagrams") {
    Expect.value(Anagram.find("solemn", ["lemons", "cherry", "melons"])).toEqual([
      "lemons",
      "melons",
    ])
  }

  do.test("does not detect anagram subsets") {
    Expect.value(Anagram.find("good", ["dog", "goody"])).toEqual([])
  }

  do.test("detects anagram") {
    Expect.value(
      Anagram.find("listen", ["enlists", "google", "inlets", "banana"])
    ).toEqual(["inlets"])
  }

  do.test("detects three anagrams") {
    Expect.value(
      Anagram.find("allergy", [
        "gallery",
        "ballerina",
        "regally",
        "clergy",
        "largely",
        "leading",
      ])
    ).toEqual(["gallery", "regally", "largely"])
  }

  do.test("detects multiple anagrams with different case") {
    Expect.value(Anagram.find("nose", ["Eons", "ONES"])).toEqual(["Eons", "ONES"])
  }

  do.test("does not detect non-anagrams with identical checksum") {
    Expect.value(Anagram.find("mass", ["last"])).toEqual([])
  }

  do.test("detects anagrams case-insensitively") {
    Expect.value(
      Anagram.find("Orchestra", ["cashregister", "Carthorse", "radishes"])
    ).toEqual(["Carthorse"])
  }

  do.test("detects anagrams using case-insensitive subject") {
    Expect.value(
      Anagram.find("Orchestra", ["cashregister", "carthorse", "radishes"])
    ).toEqual(["carthorse"])
  }

  do.test("detects anagrams using case-insensitive possible matches") {
    Expect.value(
      Anagram.find("orchestra", ["cashregister", "Carthorse", "radishes"])
    ).toEqual(["Carthorse"])
  }

  do.test("does not detect an anagram if the original word is repeated") {
    Expect.value(Anagram.find("go", ["goGoGO"])).toEqual([])
  }

  do.test("anagrams must use all letters exactly once") {
    Expect.value(Anagram.find("tapper", ["patter"])).toEqual([])
  }

  do.test("words are not anagrams of themselves") {
    Expect.value(Anagram.find("BANANA", ["BANANA"])).toEqual([])
  }

  do.test("words are not anagrams of themselves even if letter case is partially different") {
    Expect.value(Anagram.find("BANANA", ["Banana"])).toEqual([])
  }

  do.test("words are not anagrams of themselves even if letter case is completely different") {
    Expect.value(Anagram.find("BANANA", ["banana"])).toEqual([])
  }

  do.test("words other than themselves can be anagrams") {
    Expect.value(Anagram.find("LISTEN", ["LISTEN", "Silent"])).toEqual([
      "Silent",
    ])
  }
}
