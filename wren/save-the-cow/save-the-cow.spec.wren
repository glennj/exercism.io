import "./save-the-cow" for SaveTheCow
import "wren-testie/testie" for Testie, Expect

Testie.test("SaveTheCow") { |do, skip|
  do.test("Initially 9 failures are allowed and no letters are guessed") {
    var guesses = []
    var expected = {"state": "Ongoing", "maskedWord": "____", "remainingFailures": 9}
    Expect.value(SaveTheCow.guess("loot", guesses)).toEqual(expected)
  }

  do.test("After 10 failures the game is over") {
    var guesses = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]
    var expected = {"state": "Lose", "maskedWord": "____", "remainingFailures": 0}
    Expect.value(SaveTheCow.guess("loot", guesses)).toEqual(expected)
  }

  do.test("Losing with several correct guesses") {
    var guesses = ["t", "o", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]
    var expected = {"state": "Lose", "maskedWord": "_oot", "remainingFailures": 0}
    Expect.value(SaveTheCow.guess("loot", guesses)).toEqual(expected)
  }

  do.test("Feeding a correct letter removes underscores") {
    var guesses = ["t"]
    var expected = {"state": "Ongoing", "maskedWord": "___t", "remainingFailures": 9}
    Expect.value(SaveTheCow.guess("loot", guesses)).toEqual(expected)
  }

  do.test("Feeding a correct letter twice counts as a failure") {
    var guesses = ["t", "t"]
    var expected = {"state": "Ongoing", "maskedWord": "___t", "remainingFailures": 8}
    Expect.value(SaveTheCow.guess("loot", guesses)).toEqual(expected)
  }

  do.test("Guessing a repeated letter reveals all instances") {
    var guesses = ["t", "t", "o"]
    var expected = {"state": "Ongoing", "maskedWord": "_oot", "remainingFailures": 8}
    Expect.value(SaveTheCow.guess("loot", guesses)).toEqual(expected)
  }

  do.test("Getting all the letters right makes for a win") {
    var guesses = ["t", "t", "o", "l"]
    var expected = {"state": "Win", "maskedWord": "loot", "remainingFailures": 8}
    Expect.value(SaveTheCow.guess("loot", guesses)).toEqual(expected)
  }

  do.test("Winning on the last guess is still a win") {
    var guesses = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "t", "o", "l"]
    var expected = {"state": "Win", "maskedWord": "loot", "remainingFailures": 0}
    Expect.value(SaveTheCow.guess("loot", guesses)).toEqual(expected)
  }

  do.test("Guessing after a lose is error") {
    var guesses = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"]
    Expect.that {
      SaveTheCow.guess("loot", guesses)
    }.abortsWith("cannot guess after the game is lost")
  }

  do.test("Guessing after a win is error") {
    var guesses = ["t", "o", "l", "l"]
    Expect.that {
      SaveTheCow.guess("loot", guesses)
    }.abortsWith("cannot guess after the game is won")
  }
}
