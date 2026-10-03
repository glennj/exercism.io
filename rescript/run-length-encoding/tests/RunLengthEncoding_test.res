open Test
open RunLengthEncoding

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("empty string", () => {equal(~message="empty string", encode(""), "")})

test("single characters only are encoded without count", () => {
  equal(~message="single characters only are encoded without count", encode("XYZ"), "XYZ")
})

test("string with no single characters", () => {
  equal(~message="string with no single characters", encode("AABBBCCCC"), "2A3B4C")
})

test("single characters mixed with repeated characters", () => {
  equal(
    ~message="single characters mixed with repeated characters",
    encode("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"),
    "12WB12W3B24WB",
  )
})

test("multiple whitespace mixed in string", () => {
  equal(~message="multiple whitespace mixed in string", encode("  hsqq qww  "), "2 hs2q q2w2 ")
})

test("lowercase characters", () => {
  equal(~message="lowercase characters", encode("aabbbcccc"), "2a3b4c")
})

test("empty string", () => {equal(~message="empty string", decode(""), "")})

test("single characters only", () => {
  equal(~message="single characters only", decode("XYZ"), "XYZ")
})

test("string with no single characters", () => {
  equal(~message="string with no single characters", decode("2A3B4C"), "AABBBCCCC")
})

test("single characters with repeated characters", () => {
  equal(
    ~message="single characters with repeated characters",
    decode("12WB12W3B24WB"),
    "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB",
  )
})

test("multiple whitespace mixed in string", () => {
  equal(~message="multiple whitespace mixed in string", decode("2 hs2q q2w2 "), "  hsqq qww  ")
})

test("lowercase string", () => {equal(~message="lowercase string", decode("2a3b4c"), "aabbbcccc")})

test("encode followed by decode gives original string", () => {
  equal(
    ~message="encode followed by decode gives original string",
    "zzz ZZ  zZ"->encode->decode,
    "zzz ZZ  zZ",
  )
})
