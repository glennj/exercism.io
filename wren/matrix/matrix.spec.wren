import "./matrix" for Matrix
import "wren-testie/testie" for Testie, Expect

Testie.test("Matrix") { |do, skip|
  do.test("extract row from one number matrix") {
    var string = "1"
    Expect.value(Matrix.row(string, 1)).toEqual([1])
  }

  skip.test("can extract row") {
    var string = "1 2\n3 4"
    Expect.value(Matrix.row(string, 2)).toEqual([3, 4])
  }

  skip.test("extract row where numbers have different widths") {
    var string = "1 2\n10 20"
    Expect.value(Matrix.row(string, 2)).toEqual([10, 20])
  }

  skip.test("can extract row from non-square matrix with no corresponding column") {
    var string = "1 2 3\n4 5 6\n7 8 9\n8 7 6"
    Expect.value(Matrix.row(string, 4)).toEqual([8, 7, 6])
  }

  skip.test("extract column from one number matrix") {
    var string = "1"
    Expect.value(Matrix.column(string, 1)).toEqual([1])
  }

  skip.test("can extract column") {
    var string = "1 2 3\n4 5 6\n7 8 9"
    Expect.value(Matrix.column(string, 3)).toEqual([3, 6, 9])
  }

  skip.test("can extract column from non-square matrix with no corresponding row") {
    var string = "1 2 3 4\n5 6 7 8\n9 8 7 6"
    Expect.value(Matrix.column(string, 4)).toEqual([4, 8, 6])
  }

  skip.test("extract column where numbers have different widths") {
    var string = "89 1903 3\n18 3 1\n9 4 800"
    Expect.value(Matrix.column(string, 2)).toEqual([1903, 3, 4])
  }
}
