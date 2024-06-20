import "./yacht" for Yacht
import "wren-testie/testie" for Testie, Expect

Testie.test("Yacht") { |do, skip|
  do.test("Yacht") {
    var dice = [5, 5, 5, 5, 5]
    var category = "yacht"
    var expected = 50
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Not Yacht") {
    var dice = [1, 3, 3, 2, 5]
    var category = "yacht"
    var expected = 0
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Ones") {
    var dice = [1, 1, 1, 3, 5]
    var category = "ones"
    var expected = 3
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Ones, out of order") {
    var dice = [3, 1, 1, 5, 1]
    var category = "ones"
    var expected = 3
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("No ones") {
    var dice = [4, 3, 6, 5, 5]
    var category = "ones"
    var expected = 0
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Twos") {
    var dice = [2, 3, 4, 5, 6]
    var category = "twos"
    var expected = 2
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Fours") {
    var dice = [1, 4, 1, 4, 1]
    var category = "fours"
    var expected = 8
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Yacht counted as threes") {
    var dice = [3, 3, 3, 3, 3]
    var category = "threes"
    var expected = 15
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Yacht of 3s counted as fives") {
    var dice = [3, 3, 3, 3, 3]
    var category = "fives"
    var expected = 0
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Fives") {
    var dice = [1, 5, 3, 5, 3]
    var category = "fives"
    var expected = 10
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Sixes") {
    var dice = [2, 3, 4, 5, 6]
    var category = "sixes"
    var expected = 6
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Full house two small, three big") {
    var dice = [2, 2, 4, 4, 4]
    var category = "full house"
    var expected = 16
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Full house three small, two big") {
    var dice = [5, 3, 3, 5, 3]
    var category = "full house"
    var expected = 19
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Two pair is not a full house") {
    var dice = [2, 2, 4, 4, 5]
    var category = "full house"
    var expected = 0
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Four of a kind is not a full house") {
    var dice = [1, 4, 4, 4, 4]
    var category = "full house"
    var expected = 0
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Yacht is not a full house") {
    var dice = [2, 2, 2, 2, 2]
    var category = "full house"
    var expected = 0
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Four of a Kind") {
    var dice = [6, 6, 4, 6, 6]
    var category = "four of a kind"
    var expected = 24
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Yacht can be scored as Four of a Kind") {
    var dice = [3, 3, 3, 3, 3]
    var category = "four of a kind"
    var expected = 12
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Full house is not Four of a Kind") {
    var dice = [3, 3, 3, 5, 5]
    var category = "four of a kind"
    var expected = 0
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Little Straight") {
    var dice = [3, 5, 4, 1, 2]
    var category = "little straight"
    var expected = 30
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Little Straight as Big Straight") {
    var dice = [1, 2, 3, 4, 5]
    var category = "big straight"
    var expected = 0
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Four in order but not a little straight") {
    var dice = [1, 1, 2, 3, 4]
    var category = "little straight"
    var expected = 0
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("No pairs but not a little straight") {
    var dice = [1, 2, 3, 4, 6]
    var category = "little straight"
    var expected = 0
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Minimum is 1, maximum is 5, but not a little straight") {
    var dice = [1, 1, 3, 4, 5]
    var category = "little straight"
    var expected = 0
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Big Straight") {
    var dice = [4, 6, 2, 5, 3]
    var category = "big straight"
    var expected = 30
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Big Straight as little straight") {
    var dice = [6, 5, 4, 3, 2]
    var category = "little straight"
    var expected = 0
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("No pairs but not a big straight") {
    var dice = [6, 5, 4, 3, 1]
    var category = "big straight"
    var expected = 0
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Choice") {
    var dice = [3, 3, 5, 6, 6]
    var category = "choice"
    var expected = 23
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }

  do.test("Yacht as choice") {
    var dice = [2, 2, 2, 2, 2]
    var category = "choice"
    var expected = 10
    Expect.value(Yacht.score(dice, category)).toEqual(expected)
  }
}
