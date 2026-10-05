open Test
open EliudsEggs

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("0 eggs", () => {equal(~message="0 eggs", eggCount(0), 0)})

test("1 egg", () => {equal(~message="1 egg", eggCount(16), 1)})

test("4 eggs", () => {equal(~message="4 eggs", eggCount(89), 4)})

test("13 eggs", () => {equal(~message="13 eggs", eggCount(2000000000), 13)})
