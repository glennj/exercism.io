import "./anagram" for Anagram
import "wren-testie/testie" for Testie, Expect

Testie.test("Anagram") { |do, skip|
  do.test("no matches") {
    Expect.value(
      Anagram.find("diaper", ["hello", "world", "zombies", "pants"])
    ).toEqual([])
  }

  skip.test("detects two anagrams") {
    Expect.value(Anagram.find("solemn", ["lemons", "cherry", "melons"])).toEqual([
      "lemons",
      "melons",
    ])
  }

  skip.test("does not detect anagram subsets") {
    Expect.value(Anagram.find("good", ["dog", "goody"])).toEqual([])
  }

  skip.test("detects anagram") {
    Expect.value(
      Anagram.find("listen", ["enlists", "google", "inlets", "banana"])
    ).toEqual(["inlets"])
  }

  skip.test("detects three anagrams") {
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

  skip.test("detects multiple anagrams with different case") {
    Expect.value(Anagram.find("nose", ["Eons", "ONES"])).toEqual(["Eons", "ONES"])
  }

  skip.test("does not detect non-anagrams with identical checksum") {
    Expect.value(Anagram.find("mass", ["last"])).toEqual([])
  }

  skip.test("detects anagrams case-insensitively") {
    Expect.value(
      Anagram.find("Orchestra", ["cashregister", "Carthorse", "radishes"])
    ).toEqual(["Carthorse"])
  }

  skip.test("detects anagrams using case-insensitive subject") {
    Expect.value(
      Anagram.find("Orchestra", ["cashregister", "carthorse", "radishes"])
    ).toEqual(["carthorse"])
  }

  skip.test("detects anagrams using case-insensitive possible matches") {
    Expect.value(
      Anagram.find("orchestra", ["cashregister", "Carthorse", "radishes"])
    ).toEqual(["Carthorse"])
  }

  skip.test("does not detect an anagram if the original word is repeated") {
    Expect.value(Anagram.find("go", ["go Go GO"])).toEqual([])
  }

  skip.test("anagrams must use all letters exactly once") {
    Expect.value(Anagram.find("tapper", ["patter"])).toEqual([])
  }

  skip.test("words are not anagrams of themselves (case-insensitive)") {
    Expect.value(Anagram.find("BANANA", ["BANANA", "Banana", "banana"])).toEqual([])
  }

  skip.test("words other than themselves can be anagrams") {
    Expect.value(Anagram.find("LISTEN", ["Listen", "Silent", "LISTEN"])).toEqual([
      "Silent",
    ])
  }
}
