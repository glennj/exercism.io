import "./wordy" for Wordy
import "wren-testie/testie" for Testie, Expect

Testie.test("Wordy") { |do, skip|
  do.test("just a number") {
    Expect.value(Wordy.answer("What is 5?")).toEqual(5)
  }

  do.test("addition") {
    Expect.value(Wordy.answer("What is 1 plus 1?")).toEqual(2)
  }

  do.test("more addition") {
    Expect.value(Wordy.answer("What is 53 plus 2?")).toEqual(55)
  }

  do.test("addition with negative numbers") {
    Expect.value(Wordy.answer("What is -1 plus -10?")).toEqual(-11)
  }

  do.test("large addition") {
    Expect.value(Wordy.answer("What is 123 plus 45678?")).toEqual(45801)
  }

  do.test("subtraction") {
    Expect.value(Wordy.answer("What is 4 minus -12?")).toEqual(16)
  }

  do.test("multiplication") {
    Expect.value(Wordy.answer("What is -3 multiplied by 25?")).toEqual(-75)
  }

  do.test("division") {
    Expect.value(Wordy.answer("What is 33 divided by -3?")).toEqual(-11)
  }

  do.test("multiple additions") {
    Expect.value(Wordy.answer("What is 1 plus 1 plus 1?")).toEqual(3)
  }

  do.test("addition and subtraction") {
    Expect.value(Wordy.answer("What is 1 plus 5 minus -2?")).toEqual(8)
  }

  do.test("multiple subtraction") {
    Expect.value(Wordy.answer("What is 20 minus 4 minus 13?")).toEqual(3)
  }

  do.test("subtraction then addition") {
    Expect.value(Wordy.answer("What is 17 minus 6 plus 3?")).toEqual(14)
  }

  do.test("multiple multiplication") {
    Expect.value(Wordy.answer("What is 2 multiplied by -2 multiplied by 3?")).toEqual(-12)
  }

  do.test("addition and multiplication") {
    Expect.value(Wordy.answer("What is -3 plus 7 multiplied by -2?")).toEqual(-8)
  }

  do.test("multiple division") {
    Expect.value(Wordy.answer("What is -12 divided by 2 divided by -3?")).toEqual(2)
  }

  do.test("unknown operation") {
    Expect.that { Wordy.answer("What is 52 cubed?") }.abortsWith(
      "Unknown operation"
    )
  }

  do.test("Non math question") {
    Expect.that { Wordy.answer("Who is the President of the United States?") }.abortsWith(
      "Unknown operation"
    )
  }

  do.test("reject problem missing an operand") {
    Expect.that { Wordy.answer("What is 1 plus?") }.abortsWith(
      "Syntax error"
    )
  }

  do.test("reject problem with no operands or operators") {
    Expect.that { Wordy.answer("What is?") }.abortsWith(
      "Syntax error"
    )
  }

  do.test("reject two operations in a row") {
    Expect.that { Wordy.answer("What is 1 plus plus 2?") }.abortsWith(
      "Syntax error"
    )
  }

  do.test("reject two numbers in a row") {
    Expect.that { Wordy.answer("What is 1 plus 2 1?") }.abortsWith(
      "Syntax error"
    )
  }

  do.test("reject postfix notation") {
    Expect.that { Wordy.answer("What is 1 2 plus?") }.abortsWith(
      "Syntax error"
    )
  }

  do.test("reject prefix notation") {
    Expect.that { Wordy.answer("What is plus 1 2?") }.abortsWith(
      "Syntax error"
    )
  }
}
