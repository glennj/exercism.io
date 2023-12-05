import "./pythagorean-triplet" for PythagoreanTriplet
import "wren-testie/testie" for Testie, Expect

Testie.test("PythagoreanTriplet") { |do, skip|
  do.test("triplets whose sum is 12") {
    Expect.value(PythagoreanTriplet.tripletsWithSum(12)).toEqual([
      [3, 4, 5],
    ])
  }

  do.test("triplets whose sum is 108") {
    Expect.value(PythagoreanTriplet.tripletsWithSum(108)).toEqual([
      [27, 36, 45],
    ])
  }

  do.test("triplets whose sum is 1000") {
    Expect.value(PythagoreanTriplet.tripletsWithSum(1000)).toEqual([
      [200, 375, 425],
    ])
  }

  do.test("no matching triplets for 1001") {
    Expect.value(PythagoreanTriplet.tripletsWithSum(1001)).toEqual([])
  }

  do.test("returns all matching triplets") {
    Expect.value(PythagoreanTriplet.tripletsWithSum(90)).toEqual([
      [9, 40, 41],
      [15, 36, 39],
    ])
  }

  do.test("several matching triplets") {
    Expect.value(PythagoreanTriplet.tripletsWithSum(840)).toEqual([
      [40, 399, 401],
      [56, 390, 394],
      [105, 360, 375],
      [120, 350, 370],
      [140, 336, 364],
      [168, 315, 357],
      [210, 280, 350],
      [240, 252, 348],
    ])
  }

  do.test("triplets for large number") {
    Expect.value(PythagoreanTriplet.tripletsWithSum(30000)).toEqual([
      [1200, 14375, 14425],
      [1875, 14000, 14125],
      [5000, 12000, 13000],
      [6000, 11250, 12750],
      [7500, 10000, 12500],
    ])
  }
}
