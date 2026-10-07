open Test
open PhoneNumber

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("cleans the number", () => {
  equal(~message="cleans the number", clean("(223) 456-7890"), Some("2234567890"))
})

test("cleans numbers with dots", () => {
  equal(~message="cleans numbers with dots", clean("223.456.7890"), Some("2234567890"))
})

test("cleans numbers with multiple spaces", () => {
  equal(
    ~message="cleans numbers with multiple spaces",
    clean("223 456   7890   "),
    Some("2234567890"),
  )
})

test("invalid when 9 digits", () => {
  equal(~message="invalid when 9 digits", clean("123456789"), None)
})

test("invalid when 11 digits does not start with a 1", () => {
  equal(~message="invalid when 11 digits does not start with a 1", clean("22234567890"), None)
})

test("valid when 11 digits and starting with 1", () => {
  equal(
    ~message="valid when 11 digits and starting with 1",
    clean("12234567890"),
    Some("2234567890"),
  )
})

test("valid when 11 digits and starting with 1 even with punctuation", () => {
  equal(
    ~message="valid when 11 digits and starting with 1 even with punctuation",
    clean("+1 (223) 456-7890"),
    Some("2234567890"),
  )
})

test("invalid when more than 11 digits", () => {
  equal(~message="invalid when more than 11 digits", clean("321234567890"), None)
})

test("invalid with letters", () => {
  equal(~message="invalid with letters", clean("523-abc-7890"), None)
})

test("invalid with punctuations", () => {
  equal(~message="invalid with punctuations", clean("523-@:!-7890"), None)
})

test("invalid if area code starts with 0", () => {
  equal(~message="invalid if area code starts with 0", clean("(023) 456-7890"), None)
})

test("invalid if area code starts with 1", () => {
  equal(~message="invalid if area code starts with 1", clean("(123) 456-7890"), None)
})

test("invalid if exchange code starts with 0", () => {
  equal(~message="invalid if exchange code starts with 0", clean("(223) 056-7890"), None)
})

test("invalid if exchange code starts with 1", () => {
  equal(~message="invalid if exchange code starts with 1", clean("(223) 156-7890"), None)
})

test("invalid if area code starts with 0 on valid 11-digit number", () => {
  equal(
    ~message="invalid if area code starts with 0 on valid 11-digit number",
    clean("1 (023) 456-7890"),
    None,
  )
})

test("invalid if area code starts with 1 on valid 11-digit number", () => {
  equal(
    ~message="invalid if area code starts with 1 on valid 11-digit number",
    clean("1 (123) 456-7890"),
    None,
  )
})

test("invalid if exchange code starts with 0 on valid 11-digit number", () => {
  equal(
    ~message="invalid if exchange code starts with 0 on valid 11-digit number",
    clean("1 (223) 056-7890"),
    None,
  )
})

test("invalid if exchange code starts with 1 on valid 11-digit number", () => {
  equal(
    ~message="invalid if exchange code starts with 1 on valid 11-digit number",
    clean("1 (223) 156-7890"),
    None,
  )
})
