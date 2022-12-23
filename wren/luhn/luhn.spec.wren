import "./luhn" for Luhn
import "wren-testie/testie" for Testie, Expect

Testie.test("Luhn") { |do, skip|
  do.test("single digit strings can not be valid") {
    Expect.that(Luhn.valid("1")).toEqual(false)
  }

  skip.test("a single zero is invalid") {
    Expect.that(Luhn.valid("0")).toEqual(false)
  }

  skip.test("a simple valid SIN that remains valid if reversed") {
    Expect.that(Luhn.valid("059")).toEqual(true)
  }

  skip.test("a simple valid SIN that becomes invalid if reversed") {
    Expect.that(Luhn.valid("59")).toEqual(true)
  }

  skip.test("a valid Canadian SIN") {
    Expect.that(Luhn.valid("055 444 285")).toEqual(true)
  }

  skip.test("invalid Canadian SIN") {
    Expect.that(Luhn.valid("055 444 286")).toEqual(false)
  }

  skip.test("invalid credit card") {
    Expect.that(Luhn.valid("8273 1232 7352 0569")).toEqual(false)
  }

  skip.test("invalid long number with an even remainder") {
    Expect.that(Luhn.valid("1 2345 6789 1234 5678 9012")).toEqual(false)
  }

  skip.test("valid number with an even number of digits") {
    Expect.that(Luhn.valid("095 245 88")).toEqual(true)
  }

  skip.test("valid number with an odd number of spaces") {
    Expect.that(Luhn.valid("234 567 891 234")).toEqual(true)
  }

  skip.test("valid strings with a non-digit added at the end invalid") {
    Expect.that(Luhn.valid("059a")).toEqual(false)
  }

  skip.test("valid strings with punctuation included become invalid") {
    Expect.that(Luhn.valid("055-444-285")).toEqual(false)
  }

  skip.test("valid strings with symbols included become invalid") {
    Expect.that(Luhn.valid("055# 444$ 285")).toEqual(false)
  }

  skip.test("single zero with space is invalid") {
    Expect.that(Luhn.valid(" 0")).toEqual(false)
  }

  skip.test("more than a single zero is valid") {
    Expect.that(Luhn.valid("0000 0")).toEqual(true)
  }

  skip.test("input digit 9 is correctly converted to output digit 9") {
    Expect.that(Luhn.valid("091")).toEqual(true)
  }

  skip.test("using ascii value for non-doubled non-digit isn't allowed") {
    Expect.that(Luhn.valid("055b 444 285")).toEqual(false)
  }

  skip.test("using ascii value for doubled non-digit isn't allowed") {
    Expect.that(Luhn.valid(":9")).toEqual(false)
  }

  skip.test("non-numeric, non-space char in the middle with a sum that's divisible by 10 isn't allowed") {
    Expect.that(Luhn.valid("59!59")).toEqual(false)
  }
}
