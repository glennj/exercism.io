import "./killer-sudoku-helper" for KillerSudokuHelper
import "wren-testie/testie" for Testie, Expect

Testie.test("KillerSudokuHelper") { |do, skip|
  do.test("Trivial 1-digit cages -> 1") {
    Expect.value(KillerSudokuHelper.combinations(1, 1, [])).toEqual([[1]])
  }

  do.test("Trivial 1-digit cages -> 2") {
    Expect.value(KillerSudokuHelper.combinations(2, 1, [])).toEqual([[2]])
  }

  do.test("Trivial 1-digit cages -> 3") {
    Expect.value(KillerSudokuHelper.combinations(3, 1, [])).toEqual([[3]])
  }

  do.test("Trivial 1-digit cages -> 4") {
    Expect.value(KillerSudokuHelper.combinations(4, 1, [])).toEqual([[4]])
  }

  do.test("Trivial 1-digit cages -> 5") {
    Expect.value(KillerSudokuHelper.combinations(5, 1, [])).toEqual([[5]])
  }

  do.test("Trivial 1-digit cages -> 6") {
    Expect.value(KillerSudokuHelper.combinations(6, 1, [])).toEqual([[6]])
  }

  do.test("Trivial 1-digit cages -> 7") {
    Expect.value(KillerSudokuHelper.combinations(7, 1, [])).toEqual([[7]])
  }

  do.test("Trivial 1-digit cages -> 8") {
    Expect.value(KillerSudokuHelper.combinations(8, 1, [])).toEqual([[8]])
  }

  do.test("Trivial 1-digit cages -> 9") {
    Expect.value(KillerSudokuHelper.combinations(9, 1, [])).toEqual([[9]])
  }

  do.test("Cage with sum 45 contains all digits 1:9") {
    Expect.value(KillerSudokuHelper.combinations(45, 9, [])).toEqual([[1, 2, 3, 4, 5, 6, 7, 8, 9]])
  }

  do.test("Cage with only 1 possible combination") {
    Expect.value(KillerSudokuHelper.combinations(7, 3, [])).toEqual([[1, 2, 4]])
  }

  do.test("Cage with several combinations") {
    Expect.value(KillerSudokuHelper.combinations(10, 2, [])).toEqual([[1, 9], [2, 8], [3, 7], [4, 6]])
  }

  do.test("Cage with several combinations that is restricted") {
    Expect.value(KillerSudokuHelper.combinations(10, 2, [1, 4])).toEqual([[2, 8], [3, 7]])
  }
}
