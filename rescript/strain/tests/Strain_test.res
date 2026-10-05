open Test
open Strain

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("keep on empty list returns empty list", () => {
  equal(~message="keep on empty list returns empty list", keep([], _ => true), [])
})

test("keeps everything", () => {
  equal(~message="keeps everything", keep([1, 3, 5], _ => true), [1, 3, 5])
})

test("keeps nothing", () => {equal(~message="keeps nothing", keep([1, 3, 5], _ => false), [])})

test("keeps first and last", () => {
  equal(~message="keeps first and last", keep([1, 2, 3], x => mod(x, 2) == 1), [1, 3])
})

test("keeps neither first nor last", () => {
  equal(~message="keeps neither first nor last", keep([1, 2, 3], x => mod(x, 2) == 0), [2])
})

test("keeps strings", () => {
  equal(
    ~message="keeps strings",
    keep(["apple", "zebra", "banana", "zombies", "cherimoya", "zealot"], x =>
      String.startsWith(x, "z")
    ),
    ["zebra", "zombies", "zealot"],
  )
})

test("keeps lists", () => {
  equal(
    ~message="keeps lists",
    keep([[1, 2, 3], [5, 5, 5], [5, 1, 2], [2, 1, 2], [1, 5, 2], [2, 2, 1], [1, 2, 5]], x =>
      Array.includes(x, 5)
    ),
    [[5, 5, 5], [5, 1, 2], [1, 5, 2], [1, 2, 5]],
  )
})

test("discard on empty list returns empty list", () => {
  equal(~message="discard on empty list returns empty list", discard([], _ => true), [])
})

test("discards everything", () => {
  equal(~message="discards everything", discard([1, 3, 5], _ => true), [])
})

test("discards nothing", () => {
  equal(~message="discards nothing", discard([1, 3, 5], _ => false), [1, 3, 5])
})

test("discards first and last", () => {
  equal(~message="discards first and last", discard([1, 2, 3], x => mod(x, 2) == 1), [2])
})

test("discards neither first nor last", () => {
  equal(~message="discards neither first nor last", discard([1, 2, 3], x => mod(x, 2) == 0), [1, 3])
})

test("discards strings", () => {
  equal(
    ~message="discards strings",
    discard(["apple", "zebra", "banana", "zombies", "cherimoya", "zealot"], x =>
      String.startsWith(x, "z")
    ),
    ["apple", "banana", "cherimoya"],
  )
})

test("discards lists", () => {
  equal(
    ~message="discards lists",
    discard([[1, 2, 3], [5, 5, 5], [5, 1, 2], [2, 1, 2], [1, 5, 2], [2, 2, 1], [1, 2, 5]], x =>
      Array.includes(x, 5)
    ),
    [[1, 2, 3], [2, 1, 2], [2, 2, 1]],
  )
})
