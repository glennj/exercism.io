import "./two-fer" for TwoFer
import "wren-testie/testie" for Testie, Expect

Testie.test("TwoFer") { |do, skip|
  do.test("no name given") {
    Expect.value(TwoFer.twoFer()).toEqual("One for you, one for me.")
  }

  skip.test("a name given") {
    Expect.value(TwoFer.twoFer("Alice")).toEqual("One for Alice, one for me.")
  }

  skip.test("another name given") {
    Expect.value(TwoFer.twoFer("Bob")).toEqual("One for Bob, one for me.")
  }
}
