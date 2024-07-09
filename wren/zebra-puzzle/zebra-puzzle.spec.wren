import "./zebra-puzzle" for ZebraPuzzle
import "wren-testie/testie" for Testie, Expect

Testie.test("ZebraPuzzle") { |do, skip|
  do.test("resident who drinks water") {
    var puzzle = ZebraPuzzle.new()
    var actual = puzzle.drinksWater
    var expected = "Norwegian"
    Expect.value(actual).toEqual(expected)
  }

  skip.test("resident who owns zebra") {
    var puzzle = ZebraPuzzle.new()
    var actual = puzzle.ownsZebra
    var expected = "Japanese"
    Expect.value(actual).toEqual(expected)
  }
}
