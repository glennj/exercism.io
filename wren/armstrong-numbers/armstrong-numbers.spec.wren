import "./armstrong-numbers" for Number
import "wren-testie/testie" for Testie, Expect

Testie.test("Armstrong Numbers") { |do, skip|
  do.test("Zero is an Armstrong number") {
    Expect.value(Number.isArmstrong(0)).toEqual(true)
  }

  do.test("Single digit numbers are Armstrong numbers") {
    Expect.value(Number.isArmstrong(5)).toEqual(true)
  }

  do.test("There are no 2 digit Armstrong numbers") {
    Expect.value(Number.isArmstrong(10)).toEqual(false)
  }

  do.test("Three digit number that is an Armstrong number") {
    Expect.value(Number.isArmstrong(153)).toEqual(true)
  }

  do.test("Three digit number that is not an Armstrong number") {
    Expect.value(Number.isArmstrong(100)).toEqual(false)
  }

  do.test("Four digit number that is an Armstrong number") {
    Expect.value(Number.isArmstrong(9474)).toEqual(true)
  }

  do.test("Four digit number that is not an Armstrong number") {
    Expect.value(Number.isArmstrong(9475)).toEqual(false)
  }

  do.test("Seven digit number that is an Armstrong number") {
    Expect.value(Number.isArmstrong(9926315)).toEqual(true)
  }

  do.test("Seven digit number that is not an Armstrong number") {
    Expect.value(Number.isArmstrong(9926314)).toEqual(false)
  }
}
