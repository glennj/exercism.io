import "wren-testie/testie" for Testie, Expect
import "./saddle-points" for Matrix

Testie.test("SaddlePoints") { |do, skip|
  do.test("Can identify single saddle point") {
    var matrix = Matrix.new([
      [9, 8, 7],
      [5, 3, 2],
      [6, 6, 7],
    ])
    var expected = [
      {"row": 2, "column": 1},
    ]
    Expect.value(matrix.saddlePoints()).toEqual(expected)
  }

  do.test("Can identify that empty matrix has no saddle points") {
    var matrix = Matrix.new([
      [],
    ])
    var expected = []
    Expect.value(matrix.saddlePoints()).toEqual(expected)
  }

  do.test("Can identify lack of saddle points when there are none") {
    var matrix = Matrix.new([
      [1, 2, 3],
      [3, 1, 2],
      [2, 3, 1],
    ])
    var expected = []
    Expect.value(matrix.saddlePoints()).toEqual(expected)
  }

  do.test("Can identify multiple saddle points in a column") {
    var matrix = Matrix.new([
      [4, 5, 4],
      [3, 5, 5],
      [1, 5, 4],
    ])
    var expected = [
      {"row": 1, "column": 2},
      {"row": 2, "column": 2},
      {"row": 3, "column": 2},
    ]
    Expect.value(matrix.saddlePoints()).toEqual(expected)
  }

  do.test("Can identify multiple saddle points in a row") {
    var matrix = Matrix.new([
      [6, 7, 8],
      [5, 5, 5],
      [7, 5, 6],
    ])
    var expected = [
      {"row": 2, "column": 1},
      {"row": 2, "column": 2},
      {"row": 2, "column": 3},
    ]
    Expect.value(matrix.saddlePoints()).toEqual(expected)
  }

  do.test("Can identify saddle point in bottom right corner") {
    var matrix = Matrix.new([
      [8, 7, 9],
      [6, 7, 6],
      [3, 2, 5],
    ])
    var expected = [
      {"row": 3, "column": 3},
    ]
    Expect.value(matrix.saddlePoints()).toEqual(expected)
  }

  do.test("Can identify saddle points in a non square matrix") {
    var matrix = Matrix.new([
      [3, 1, 3],
      [3, 2, 4],
    ])
    var expected = [
      {"row": 1, "column": 1},
      {"row": 1, "column": 3},
    ]
    Expect.value(matrix.saddlePoints()).toEqual(expected)
  }

  do.test("Can identify that saddle points in a single column matrix are those with the minimum value") {
    var matrix = Matrix.new([
      [2],
      [1],
      [4],
      [1],
    ])
    var expected = [
      {"row": 2, "column": 1},
      {"row": 4, "column": 1},
    ]
    Expect.value(matrix.saddlePoints()).toEqual(expected)
  }

  do.test("Can identify that saddle points in a single row matrix are those with the maximum value") {
    var matrix = Matrix.new([
      [2, 5, 3, 5],
    ])
    var expected = [
      {"row": 1, "column": 2},
      {"row": 1, "column": 4},
    ]
    Expect.value(matrix.saddlePoints()).toEqual(expected)
  }
}
