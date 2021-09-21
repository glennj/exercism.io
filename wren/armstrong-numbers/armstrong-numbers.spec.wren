import "./armstrong-numbers" for Number
import "wren-testie/testie" for Testie, Expect
import "wren-testie/reporters/tap" for TapReporter

var suite = Testie.test("Armstrong Numbers", {"reporter": TapReporter}) { |do, skip|
  do.test("Zero is an Armstrong number") {
    Expect.value(Number.isArmstrong(0)).toEqual(true)
  }

  do.test("Single digit numbers are Armstrong numbers") {
    Expect.value(Number.isArmstrong(5)).toEqual(true)
  }

  do.test("There are no 2 digit Armstrong numbers") {
    Expect.value(Number.isArmstrong(10)).toEqual(false)
  }

  skip.test("Three digit number that is an Armstrong number") {
    Expect.value(Number.isArmstrong(153)).toEqual(true)
  }

  skip.test("Three digit number that is not an Armstrong number") {
    Expect.value(Number.isArmstrong(100)).toEqual(false)
  }

  skip.test("Four digit number that is an Armstrong number") {
    Expect.value(Number.isArmstrong(9474)).toEqual(true)
  }

  skip.test("Four digit number that is not an Armstrong number") {
    Expect.value(Number.isArmstrong(9475)).toEqual(false)
  }

  skip.test("Seven digit number that is an Armstrong number") {
    Expect.value(Number.isArmstrong(9926315)).toEqual(true)
  }

  skip.test("Seven digit number that is not an Armstrong number") {
    Expect.value(Number.isArmstrong(9926314)).toEqual(false)
  }
}
