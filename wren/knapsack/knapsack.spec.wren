import "./knapsack" for Knapsack
import "wren-testie/testie" for Testie, Expect

Testie.test("Knapsack") { |do, skip|
  do.test("no items") {
    var items = []
    Expect.value(Knapsack.maximumValue(100, items)).toEqual(0)
  }

  do.test("one item, too heavy") {
    var items = [
      {"weight": 100, "value": 1},
    ]
    Expect.value(Knapsack.maximumValue(10, items)).toEqual(0)
  }

  do.test("five items (cannot be greedy by weight)") {
    var items = [
      {"weight": 2, "value": 5},
      {"weight": 2, "value": 5},
      {"weight": 2, "value": 5},
      {"weight": 2, "value": 5},
      {"weight": 10, "value": 21},
    ]
    Expect.value(Knapsack.maximumValue(10, items)).toEqual(21)
  }

  do.test("five items (cannot be greedy by value)") {
    var items = [
      {"weight": 2, "value": 20},
      {"weight": 2, "value": 20},
      {"weight": 2, "value": 20},
      {"weight": 2, "value": 20},
      {"weight": 10, "value": 50},
    ]
    Expect.value(Knapsack.maximumValue(10, items)).toEqual(80)
  }

  do.test("example knapsack") {
    var items = [
      {"weight": 5, "value": 10},
      {"weight": 4, "value": 40},
      {"weight": 6, "value": 30},
      {"weight": 4, "value": 50},
    ]
    Expect.value(Knapsack.maximumValue(10, items)).toEqual(90)
  }

  do.test("8 items") {
    var items = [
      {"weight": 25, "value": 350},
      {"weight": 35, "value": 400},
      {"weight": 45, "value": 450},
      {"weight": 5, "value": 20},
      {"weight": 25, "value": 70},
      {"weight": 3, "value": 8},
      {"weight": 2, "value": 5},
      {"weight": 2, "value": 5},
    ]
    Expect.value(Knapsack.maximumValue(104, items)).toEqual(900)
  }

  do.test("15 items") {
    var items = [
      {"weight": 70, "value": 135},
      {"weight": 73, "value": 139},
      {"weight": 77, "value": 149},
      {"weight": 80, "value": 150},
      {"weight": 82, "value": 156},
      {"weight": 87, "value": 163},
      {"weight": 90, "value": 173},
      {"weight": 94, "value": 184},
      {"weight": 98, "value": 192},
      {"weight": 106, "value": 201},
      {"weight": 110, "value": 210},
      {"weight": 113, "value": 214},
      {"weight": 115, "value": 221},
      {"weight": 118, "value": 229},
      {"weight": 120, "value": 240},
    ]
    Expect.value(Knapsack.maximumValue(750, items)).toEqual(1458)
  }
}
