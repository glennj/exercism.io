open Test
open TwoFer

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("no name given", () => {
  equal(~message="no name given", twoFer(None), "One for you, one for me.")
})

test("a name given", () => {
  equal(~message="a name given", twoFer(Some("Alice")), "One for Alice, one for me.")
})

test("another name given", () => {
  equal(~message="another name given", twoFer(Some("Bob")), "One for Bob, one for me.")
})
