open Test
open RotationalCipher

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("rotate a by 0, same output as input", () => {
  equal(~message="rotate a by 0, same output as input", rotate("a", 0), "a")
})

test("rotate a by 1", () => {equal(~message="rotate a by 1", rotate("a", 1), "b")})

test("rotate a by 26, same output as input", () => {
  equal(~message="rotate a by 26, same output as input", rotate("a", 26), "a")
})

test("rotate m by 13", () => {equal(~message="rotate m by 13", rotate("m", 13), "z")})

test("rotate n by 13 with wrap around alphabet", () => {
  equal(~message="rotate n by 13 with wrap around alphabet", rotate("n", 13), "a")
})

test("rotate capital letters", () => {
  equal(~message="rotate capital letters", rotate("OMG", 5), "TRL")
})

test("rotate spaces", () => {equal(~message="rotate spaces", rotate("O M G", 5), "T R L")})

test("rotate numbers", () => {
  equal(~message="rotate numbers", rotate("Testing 1 2 3 testing", 4), "Xiwxmrk 1 2 3 xiwxmrk")
})

test("rotate punctuation", () => {
  equal(~message="rotate punctuation", rotate("Let's eat, Grandma!", 21), "Gzo'n zvo, Bmviyhv!")
})

test("rotate all letters", () => {
  equal(
    ~message="rotate all letters",
    rotate("The quick brown fox jumps over the lazy dog.", 13),
    "Gur dhvpx oebja sbk whzcf bire gur ynml qbt.",
  )
})
