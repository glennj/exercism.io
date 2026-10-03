open Test
open ResistorColor

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("Black", () => {equal(~message="Black", colorCode("black"), 0)})

test("White", () => {equal(~message="White", colorCode("white"), 9)})

test("Orange", () => {equal(~message="Orange", colorCode("orange"), 3)})

test("Colors", () => {
  equal(
    ~message="Colors",
    colors,
    ["black", "brown", "red", "orange", "yellow", "green", "blue", "violet", "grey", "white"],
  )
})
