import "./phone-number" for PhoneNumber
import "wren-testie/testie" for Testie, Expect

Testie.test("PhoneNumber") { |do, skip|
  do.test("cleans the number") {
    var actual = PhoneNumber.clean("(223) 456-7890")
    var expected = "2234567890"
    Expect.value(actual).toEqual(expected)
  }

  do.test("cleans numbers with dots") {
    var actual = PhoneNumber.clean("223.456.7890")
    var expected = "2234567890"
    Expect.value(actual).toEqual(expected)
  }

  do.test("cleans numbers with multiple spaces") {
    var actual = PhoneNumber.clean("223 456   7890   ")
    var expected = "2234567890"
    Expect.value(actual).toEqual(expected)
  }

  do.test("invalid when 9 digits") {
    Expect.that {
      PhoneNumber.clean("123456789")
    }.abortsWith("must not be fewer than 10 digits")
  }

  do.test("invalid when 11 digits does not start with a 1") {
    Expect.that {
      PhoneNumber.clean("22234567890")
    }.abortsWith("11 digits must start with 1")
  }

  do.test("valid when 11 digits and starting with 1") {
    var actual = PhoneNumber.clean("12234567890")
    var expected = "2234567890"
    Expect.value(actual).toEqual(expected)
  }

  do.test("valid when 11 digits and starting with 1 even with punctuation") {
    var actual = PhoneNumber.clean("+1 (223) 456-7890")
    var expected = "2234567890"
    Expect.value(actual).toEqual(expected)
  }

  do.test("invalid when more than 11 digits") {
    Expect.that {
      PhoneNumber.clean("321234567890")
    }.abortsWith("must not be greater than 11 digits")
  }

  do.test("invalid with letters") {
    Expect.that {
      PhoneNumber.clean("523-abc-7890")
    }.abortsWith("letters not permitted")
  }

  do.test("invalid with punctuations") {
    Expect.that {
      PhoneNumber.clean("523-@:!-7890")
    }.abortsWith("punctuations not permitted")
  }

  do.test("invalid if area code starts with 0") {
    Expect.that {
      PhoneNumber.clean("(023) 456-7890")
    }.abortsWith("area code cannot start with zero")
  }

  do.test("invalid if area code starts with 1") {
    Expect.that {
      PhoneNumber.clean("(123) 456-7890")
    }.abortsWith("area code cannot start with one")
  }

  do.test("invalid if exchange code starts with 0") {
    Expect.that {
      PhoneNumber.clean("(223) 056-7890")
    }.abortsWith("exchange code cannot start with zero")
  }

  do.test("invalid if exchange code starts with 1") {
    Expect.that {
      PhoneNumber.clean("(223) 156-7890")
    }.abortsWith("exchange code cannot start with one")
  }

  do.test("invalid if area code starts with 0 on valid 11-digit number") {
    Expect.that {
      PhoneNumber.clean("1 (023) 456-7890")
    }.abortsWith("area code cannot start with zero")
  }

  do.test("invalid if area code starts with 1 on valid 11-digit number") {
    Expect.that {
      PhoneNumber.clean("1 (123) 456-7890")
    }.abortsWith("area code cannot start with one")
  }

  do.test("invalid if exchange code starts with 0 on valid 11-digit number") {
    Expect.that {
      PhoneNumber.clean("1 (223) 056-7890")
    }.abortsWith("exchange code cannot start with zero")
  }

  do.test("invalid if exchange code starts with 1 on valid 11-digit number") {
    Expect.that {
      PhoneNumber.clean("1 (223) 156-7890")
    }.abortsWith("exchange code cannot start with one")
  }
}
