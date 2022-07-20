import "wren-testie/testie" for Testie, Expect
import "./spiral-matrix" for SpiralMatrix

Testie.test("SpiralMatrix") { |do, skip|

  do.test("empty spiral") {
    var m = SpiralMatrix.new(0)
    Expect.value(m.toList).toEqual([])
  }

  do.test("trivial spiral") {
    var m = SpiralMatrix.new(1)
    Expect.value(m.toList).toEqual([[1]])
  }

  do.test("spiral of size 2") {
    var m = SpiralMatrix.new(2)
    Expect.value(m.toList).toEqual([
      [1, 2],
      [4, 3],
    ])
  }

  do.test("spiral of size 3") {
    var m = SpiralMatrix.new(3)
    Expect.value(m.toList).toEqual([
      [1, 2, 3],
      [8, 9, 4],
      [7, 6, 5],
    ])
  }

  do.test("spiral of size 4") {
    var m = SpiralMatrix.new(4)
    Expect.value(m.toList).toEqual([
      [ 1,  2,  3, 4],
      [12, 13, 14, 5],
      [11, 16, 15, 6],
      [10,  9,  8, 7],
    ])
  }

  do.test("spiral of size 5") {
    var m = SpiralMatrix.new(5)
    Expect.value(m.toList).toEqual([
      [ 1,  2,  3,  4, 5],
      [16, 17, 18, 19, 6],
      [15, 24, 25, 20, 7],
      [14, 23, 22, 21, 8],
      [13, 12, 11, 10, 9],
    ])
  }
}
