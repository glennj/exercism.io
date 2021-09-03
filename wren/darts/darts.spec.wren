import "./darts" for Darts
import "wren-testie/testie" for Testie, Expect

Testie.test("Darts") { |do, skip|
  do.test("Missed target") {
    Expect.value(Darts.score(-9, 9)).toEqual(0)
  }

  skip.test("On the outer circle") {
    Expect.value(Darts.score(0, 10)).toEqual(1)
  }

  skip.test("On the middle circle") {
    Expect.value(Darts.score(-5, 0)).toEqual(5)
  }

  skip.test("On the inner circle") {
    Expect.value(Darts.score(0, -1)).toEqual(10)
  }

  skip.test("Exactly on centre") {
    Expect.value(Darts.score(0, 0)).toEqual(10)
  }

  skip.test("Near the centre") {
    Expect.value(Darts.score(-0.1, -0.1)).toEqual(10)
  }

  skip.test("Just within the inner circle") {
    Expect.value(Darts.score(0.7, 0.7)).toEqual(10)
  }

  skip.test("Just outside the inner circle") {
    Expect.value(Darts.score(0.8, -0.8)).toEqual(5)
  }

  skip.test("Just within the middle circle") {
    Expect.value(Darts.score(-3.5, 3.5)).toEqual(5)
  }

  skip.test("Just outside the middle circle") {
    Expect.value(Darts.score(-3.6, -3.6)).toEqual(1)
  }

  skip.test("Just within the outer circle") {
    Expect.value(Darts.score(-7.0, 7.0)).toEqual(1)
  }

  skip.test("Just outside the outer circle") {
    Expect.value(Darts.score(7.1, -7.1)).toEqual(0)
  }

  skip.test("Asymmetric position between the inner and middle circles") {
    Expect.value(Darts.score(0.5, -4)).toEqual(5)
  }
}
