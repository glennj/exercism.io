import "./tournament" for Tournament
import "wren-testie/testie" for Testie, Expect

Testie.test("Tournament") { |do, skip|
  do.test("just the header if no input") {
    var rows = []
    var expected = [
      "Team                           | MP |  W |  D |  L |  P",
    ]
    Expect.value(Tournament.tally(rows)).toEqual(expected)
  }

  do.test("a win is three points, a loss is zero points") {
    var rows = [
      "Allegoric Alaskans;Blithering Badgers;win",
    ]
    var expected = [
      "Team                           | MP |  W |  D |  L |  P",
      "Allegoric Alaskans             |  1 |  1 |  0 |  0 |  3",
      "Blithering Badgers             |  1 |  0 |  0 |  1 |  0",
    ]
    Expect.value(Tournament.tally(rows)).toEqual(expected)
  }

  do.test("a win can also be expressed as a loss") {
    var rows = [
      "Blithering Badgers;Allegoric Alaskans;loss",
    ]
    var expected = [
      "Team                           | MP |  W |  D |  L |  P",
      "Allegoric Alaskans             |  1 |  1 |  0 |  0 |  3",
      "Blithering Badgers             |  1 |  0 |  0 |  1 |  0",
    ]
    Expect.value(Tournament.tally(rows)).toEqual(expected)
  }

  do.test("a different team can win") {
    var rows = [
      "Blithering Badgers;Allegoric Alaskans;win",
    ]
    var expected = [
      "Team                           | MP |  W |  D |  L |  P",
      "Blithering Badgers             |  1 |  1 |  0 |  0 |  3",
      "Allegoric Alaskans             |  1 |  0 |  0 |  1 |  0",
    ]
    Expect.value(Tournament.tally(rows)).toEqual(expected)
  }

  do.test("a draw is one point each") {
    var rows = [
      "Allegoric Alaskans;Blithering Badgers;draw",
    ]
    var expected = [
      "Team                           | MP |  W |  D |  L |  P",
      "Allegoric Alaskans             |  1 |  0 |  1 |  0 |  1",
      "Blithering Badgers             |  1 |  0 |  1 |  0 |  1",
    ]
    Expect.value(Tournament.tally(rows)).toEqual(expected)
  }

  do.test("There can be more than one match") {
    var rows = [
      "Allegoric Alaskans;Blithering Badgers;win",
      "Allegoric Alaskans;Blithering Badgers;win",
    ]
    var expected = [
      "Team                           | MP |  W |  D |  L |  P",
      "Allegoric Alaskans             |  2 |  2 |  0 |  0 |  6",
      "Blithering Badgers             |  2 |  0 |  0 |  2 |  0",
    ]
    Expect.value(Tournament.tally(rows)).toEqual(expected)
  }

  do.test("There can be more than one winner") {
    var rows = [
      "Allegoric Alaskans;Blithering Badgers;loss",
      "Allegoric Alaskans;Blithering Badgers;win",
    ]
    var expected = [
      "Team                           | MP |  W |  D |  L |  P",
      "Allegoric Alaskans             |  2 |  1 |  0 |  1 |  3",
      "Blithering Badgers             |  2 |  1 |  0 |  1 |  3",
    ]
    Expect.value(Tournament.tally(rows)).toEqual(expected)
  }

  do.test("There can be more than two teams") {
    var rows = [
      "Allegoric Alaskans;Blithering Badgers;win",
      "Blithering Badgers;Courageous Californians;win",
      "Courageous Californians;Allegoric Alaskans;loss",
    ]
    var expected = [
      "Team                           | MP |  W |  D |  L |  P",
      "Allegoric Alaskans             |  2 |  2 |  0 |  0 |  6",
      "Blithering Badgers             |  2 |  1 |  0 |  1 |  3",
      "Courageous Californians        |  2 |  0 |  0 |  2 |  0",
    ]
    Expect.value(Tournament.tally(rows)).toEqual(expected)
  }

  do.test("typical input") {
    var rows = [
      "Allegoric Alaskans;Blithering Badgers;win",
      "Devastating Donkeys;Courageous Californians;draw",
      "Devastating Donkeys;Allegoric Alaskans;win",
      "Courageous Californians;Blithering Badgers;loss",
      "Blithering Badgers;Devastating Donkeys;loss",
      "Allegoric Alaskans;Courageous Californians;win",
    ]
    var expected = [
      "Team                           | MP |  W |  D |  L |  P",
      "Devastating Donkeys            |  3 |  2 |  1 |  0 |  7",
      "Allegoric Alaskans             |  3 |  2 |  0 |  1 |  6",
      "Blithering Badgers             |  3 |  1 |  0 |  2 |  3",
      "Courageous Californians        |  3 |  0 |  1 |  2 |  1",
    ]
    Expect.value(Tournament.tally(rows)).toEqual(expected)
  }

  do.test("incomplete competition (not all pairs have played)") {
    var rows = [
      "Allegoric Alaskans;Blithering Badgers;loss",
      "Devastating Donkeys;Allegoric Alaskans;loss",
      "Courageous Californians;Blithering Badgers;draw",
      "Allegoric Alaskans;Courageous Californians;win",
    ]
    var expected = [
      "Team                           | MP |  W |  D |  L |  P",
      "Allegoric Alaskans             |  3 |  2 |  0 |  1 |  6",
      "Blithering Badgers             |  2 |  1 |  1 |  0 |  4",
      "Courageous Californians        |  2 |  0 |  1 |  1 |  1",
      "Devastating Donkeys            |  1 |  0 |  0 |  1 |  0",
    ]
    Expect.value(Tournament.tally(rows)).toEqual(expected)
  }

  do.test("ties broken alphabetically") {
    var rows = [
      "Courageous Californians;Devastating Donkeys;win",
      "Allegoric Alaskans;Blithering Badgers;win",
      "Devastating Donkeys;Allegoric Alaskans;loss",
      "Courageous Californians;Blithering Badgers;win",
      "Blithering Badgers;Devastating Donkeys;draw",
      "Allegoric Alaskans;Courageous Californians;draw",
    ]
    var expected = [
      "Team                           | MP |  W |  D |  L |  P",
      "Allegoric Alaskans             |  3 |  2 |  1 |  0 |  7",
      "Courageous Californians        |  3 |  2 |  1 |  0 |  7",
      "Blithering Badgers             |  3 |  0 |  1 |  2 |  1",
      "Devastating Donkeys            |  3 |  0 |  1 |  2 |  1",
    ]
    Expect.value(Tournament.tally(rows)).toEqual(expected)
  }

  do.test("ensure points sorted numerically") {
    var rows = [
      "Devastating Donkeys;Blithering Badgers;win",
      "Devastating Donkeys;Blithering Badgers;win",
      "Devastating Donkeys;Blithering Badgers;win",
      "Devastating Donkeys;Blithering Badgers;win",
      "Blithering Badgers;Devastating Donkeys;win",
    ]
    var expected = [
      "Team                           | MP |  W |  D |  L |  P",
      "Devastating Donkeys            |  5 |  4 |  0 |  1 | 12",
      "Blithering Badgers             |  5 |  1 |  0 |  4 |  3",
    ]
    Expect.value(Tournament.tally(rows)).toEqual(expected)
  }
}
