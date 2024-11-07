import "./game-of-life" for GameOfLife
import "wren-testie/testie" for Testie, Expect

Testie.test("GameOfLife") { |do, skip|
  do.test("empty matrix") {
    var matrix = [
    ]
    var expected = [
    ]
    Expect.value(GameOfLife.tick(matrix)).toEqual(expected)
  }

  do.test("live cells with zero live neighbors die") {
    var matrix = [
      [0, 0, 0],
      [0, 1, 0],
      [0, 0, 0],
    ]
    var expected = [
      [0, 0, 0],
      [0, 0, 0],
      [0, 0, 0],
    ]
    Expect.value(GameOfLife.tick(matrix)).toEqual(expected)
  }

  do.test("live cells with only one live neighbor die") {
    var matrix = [
      [0, 0, 0],
      [0, 1, 0],
      [0, 1, 0],
    ]
    var expected = [
      [0, 0, 0],
      [0, 0, 0],
      [0, 0, 0],
    ]
    Expect.value(GameOfLife.tick(matrix)).toEqual(expected)
  }

  do.test("live cells with two live neighbors stay alive") {
    var matrix = [
      [1, 0, 1],
      [1, 0, 1],
      [1, 0, 1],
    ]
    var expected = [
      [0, 0, 0],
      [1, 0, 1],
      [0, 0, 0],
    ]
    Expect.value(GameOfLife.tick(matrix)).toEqual(expected)
  }

  do.test("live cells with three live neighbors stay alive") {
    var matrix = [
      [0, 1, 0],
      [1, 0, 0],
      [1, 1, 0],
    ]
    var expected = [
      [0, 0, 0],
      [1, 0, 0],
      [1, 1, 0],
    ]
    Expect.value(GameOfLife.tick(matrix)).toEqual(expected)
  }

  do.test("dead cells with three live neighbors become alive") {
    var matrix = [
      [1, 1, 0],
      [0, 0, 0],
      [1, 0, 0],
    ]
    var expected = [
      [0, 0, 0],
      [1, 1, 0],
      [0, 0, 0],
    ]
    Expect.value(GameOfLife.tick(matrix)).toEqual(expected)
  }

  do.test("live cells with four or more neighbors die") {
    var matrix = [
      [1, 1, 1],
      [1, 1, 1],
      [1, 1, 1],
    ]
    var expected = [
      [1, 0, 1],
      [0, 0, 0],
      [1, 0, 1],
    ]
    Expect.value(GameOfLife.tick(matrix)).toEqual(expected)
  }

  do.test("bigger matrix") {
    var matrix = [
      [1, 1, 0, 1, 1, 0, 0, 0],
      [1, 0, 1, 1, 0, 0, 0, 0],
      [1, 1, 1, 0, 0, 1, 1, 1],
      [0, 0, 0, 0, 0, 1, 1, 0],
      [1, 0, 0, 0, 1, 1, 0, 0],
      [1, 1, 0, 0, 0, 1, 1, 1],
      [0, 0, 1, 0, 1, 0, 0, 1],
      [1, 0, 0, 0, 0, 0, 1, 1],
    ]
    var expected = [
      [1, 1, 0, 1, 1, 0, 0, 0],
      [0, 0, 0, 0, 0, 1, 1, 0],
      [1, 0, 1, 1, 1, 1, 0, 1],
      [1, 0, 0, 0, 0, 0, 0, 1],
      [1, 1, 0, 0, 1, 0, 0, 1],
      [1, 1, 0, 1, 0, 0, 0, 1],
      [1, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 1, 1],
    ]
    Expect.value(GameOfLife.tick(matrix)).toEqual(expected)
  }
}
