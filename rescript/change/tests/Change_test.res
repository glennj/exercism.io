open Test
open Change

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("change for 1 cent", () => {
  equal(~message="change for 1 cent", findFewestCoins([1, 5, 10, 25], 1), Some([1]))
})

test("single coin change", () => {
  equal(~message="single coin change", findFewestCoins([1, 5, 10, 25, 100], 25), Some([25]))
})

test("multiple coin change", () => {
  equal(~message="multiple coin change", findFewestCoins([1, 5, 10, 25, 100], 15), Some([5, 10]))
})

test("change with Lilliputian Coins", () => {
  equal(
    ~message="change with Lilliputian Coins",
    findFewestCoins([1, 4, 15, 20, 50], 23),
    Some([4, 4, 15]),
  )
})

test("change with Lower Elbonia Coins", () => {
  equal(
    ~message="change with Lower Elbonia Coins",
    findFewestCoins([1, 5, 10, 21, 25], 63),
    Some([21, 21, 21]),
  )
})

test("large target values", () => {
  equal(
    ~message="large target values",
    findFewestCoins([1, 2, 5, 10, 20, 50, 100], 999),
    Some([2, 2, 5, 20, 20, 50, 100, 100, 100, 100, 100, 100, 100, 100, 100]),
  )
})

test("possible change without unit coins available", () => {
  equal(
    ~message="possible change without unit coins available",
    findFewestCoins([2, 5, 10, 20, 50], 21),
    Some([2, 2, 2, 5, 10]),
  )
})

test("another possible change without unit coins available", () => {
  equal(
    ~message="another possible change without unit coins available",
    findFewestCoins([4, 5], 27),
    Some([4, 4, 4, 5, 5, 5]),
  )
})

test("a greedy approach is not optimal", () => {
  equal(
    ~message="a greedy approach is not optimal",
    findFewestCoins([1, 10, 11], 20),
    Some([10, 10]),
  )
})

test("no coins make 0 change", () => {
  equal(~message="no coins make 0 change", findFewestCoins([1, 5, 10, 21, 25], 0), Some([]))
})

test("error testing for change smaller than the smallest of coins", () => {
  equal(
    ~message="error testing for change smaller than the smallest of coins",
    findFewestCoins([5, 10], 3),
    None,
  )
})

test("error if no combination can add up to target", () => {
  equal(~message="error if no combination can add up to target", findFewestCoins([5, 10], 94), None)
})

test("cannot find negative change values", () => {
  equal(~message="cannot find negative change values", findFewestCoins([1, 2, 5], -5), None)
})
