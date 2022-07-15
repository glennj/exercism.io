import "./wordy" for Wordy
import "wren-testie/testie" for Testie, Expect

Testie.test("Wordy") { |do, skip|
  do.test("just a number") {
    Expect.value(Wordy.answer("What is 5?")).toEqual(5)
  }

  skip.test("addition") {
    Expect.value(Wordy.answer("What is 1 plus 1?")).toEqual(2)
  }

  skip.test("more addition") {
    Expect.value(Wordy.answer("What is 53 plus 2?")).toEqual(55)
  }

  skip.test("addition with negative numbers") {
    Expect.value(Wordy.answer("What is -1 plus -10?")).toEqual(-11)
  }

  skip.test("large addition") {
    Expect.value(Wordy.answer("What is 123 plus 45678?")).toEqual(45801)
  }

  skip.test("subtraction") {
    Expect.value(Wordy.answer("What is 4 minus -12?")).toEqual(16)
  }

  skip.test("multiplication") {
    Expect.value(Wordy.answer("What is -3 multiplied by 25?")).toEqual(-75)
  }

  skip.test("division") {
    Expect.value(Wordy.answer("What is 33 divided by -3?")).toEqual(-11)
  }

  skip.test("multiple additions") {
    Expect.value(Wordy.answer("What is 1 plus 1 plus 1?")).toEqual(3)
  }

  skip.test("addition and subtraction") {
    Expect.value(Wordy.answer("What is 1 plus 5 minus -2?")).toEqual(8)
  }

  skip.test("multiple subtraction") {
    Expect.value(Wordy.answer("What is 20 minus 4 minus 13?")).toEqual(3)
  }

  skip.test("subtraction then addition") {
    Expect.value(Wordy.answer("What is 17 minus 6 plus 3?")).toEqual(14)
  }

  skip.test("multiple multiplication") {
    Expect.value(Wordy.answer("What is 2 multiplied by -2 multiplied by 3?")).toEqual(-12)
  }

  skip.test("addition and multiplication") {
    Expect.value(Wordy.answer("What is -3 plus 7 multiplied by -2?")).toEqual(-8)
  }

  skip.test("multiple division") {
    Expect.value(Wordy.answer("What is -12 divided by 2 divided by -3?")).toEqual(2)
  }

  skip.test("unknown operation") {
    Expect.that { Wordy.answer("What is 52 cubed?") }.abortsWith(
      "Unknown operation"
    )
  }

  skip.test("Non math question") {
    Expect.that { Wordy.answer("Who is the President of the United States?") }.abortsWith(
      "Unknown operation"
    )
  }

  skip.test("reject problem missing an operand") {
    Expect.that { Wordy.answer("What is 1 plus?") }.abortsWith(
      "Syntax error"
    )
  }

  skip.test("reject problem with no operands or operators") {
    Expect.that { Wordy.answer("What is?") }.abortsWith(
      "Syntax error"
    )
  }

  skip.test("reject two operations in a row") {
    Expect.that { Wordy.answer("What is 1 plus plus 2?") }.abortsWith(
      "Syntax error"
    )
  }

  skip.test("reject two numbers in a row") {
    Expect.that { Wordy.answer("What is 1 plus 2 1?") }.abortsWith(
      "Syntax error"
    )
  }

  skip.test("reject postfix notation") {
    Expect.that { Wordy.answer("What is 1 2 plus?") }.abortsWith(
      "Syntax error"
    )
  }

  skip.test("reject prefix notation") {
    Expect.that { Wordy.answer("What is plus 1 2?") }.abortsWith(
      "Syntax error"
    )
  }
}
