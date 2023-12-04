import "./pangram" for Pangram
import "wren-testie/testie" for Testie, Expect

Testie.test("Pangram") { |do, skip|
  do.test("empty sentence") {
    Expect.value(Pangram.isPangram("")).toEqual(false)
  }

  do.test("perfect lower case") {
    Expect.value(Pangram.isPangram("abcdefghijklmnopqrstuvwxyz")).toEqual(true)
  }

  do.test("only lower case") {
    Expect.value(Pangram.isPangram("the quick brown fox jumps over the lazy dog")).toEqual(true)
  }

  do.test("missing the letter 'x'") {
    Expect.value(Pangram.isPangram("a quick movement of the enemy will jeopardize five gunboats")).toEqual(false)
  }

  do.test("missing the letter 'h'") {
    Expect.value(Pangram.isPangram("five boxing wizards jump quickly at it")).toEqual(false)
  }

  do.test("with underscores") {
    Expect.value(Pangram.isPangram("the_quick_brown_fox_jumps_over_the_lazy_dog")).toEqual(true)
  }

  do.test("with numbers") {
    Expect.value(Pangram.isPangram("the 1 quick brown fox jumps over the 2 lazy dogs")).toEqual(true)
  }

  do.test("missing letters replaced by numbers") {
    Expect.value(Pangram.isPangram("7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog")).toEqual(false)
  }

  do.test("mixed case and punctuation") {
    Expect.value(Pangram.isPangram("\"Five quacking Zephyrs jolt my wax bed.\"")).toEqual(true)
  }

  do.test("a-m and A-M are 26 different characters but not a pangram") {
    Expect.value(Pangram.isPangram("abcdefghijklm ABCDEFGHIJKLM")).toEqual(false)
  }
}
