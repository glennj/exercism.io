open Test
open Isogram

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("empty string", () => {equal(~message="empty string", isIsogram(""), true)})

test("isogram with only lower case characters", () => {
  equal(~message="isogram with only lower case characters", isIsogram("isogram"), true)
})

test("word with one duplicated character", () => {
  equal(~message="word with one duplicated character", isIsogram("eleven"), false)
})

test("word with one duplicated character from the end of the alphabet", () => {
  equal(
    ~message="word with one duplicated character from the end of the alphabet",
    isIsogram("zzyzx"),
    false,
  )
})

test("longest reported english isogram", () => {
  equal(~message="longest reported english isogram", isIsogram("subdermatoglyphic"), true)
})

test("word with duplicated character in mixed case", () => {
  equal(~message="word with duplicated character in mixed case", isIsogram("Alphabet"), false)
})

test("word with duplicated character in mixed case, lowercase first", () => {
  equal(
    ~message="word with duplicated character in mixed case, lowercase first",
    isIsogram("alphAbet"),
    false,
  )
})

test("hypothetical isogrammic word with hyphen", () => {
  equal(~message="hypothetical isogrammic word with hyphen", isIsogram("thumbscrew-japingly"), true)
})

test("hypothetical word with duplicated character following hyphen", () => {
  equal(
    ~message="hypothetical word with duplicated character following hyphen",
    isIsogram("thumbscrew-jappingly"),
    false,
  )
})

test("isogram with duplicated hyphen", () => {
  equal(~message="isogram with duplicated hyphen", isIsogram("six-year-old"), true)
})

test("made-up name that is an isogram", () => {
  equal(~message="made-up name that is an isogram", isIsogram("Emily Jung Schwartzkopf"), true)
})

test("duplicated character in the middle", () => {
  equal(~message="duplicated character in the middle", isIsogram("accentor"), false)
})

test("same first and last characters", () => {
  equal(~message="same first and last characters", isIsogram("angola"), false)
})

test("word with duplicated character and with two hyphens", () => {
  equal(
    ~message="word with duplicated character and with two hyphens",
    isIsogram("up-to-date"),
    false,
  )
})
