open Test
open ResistorColorDuo

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("Brown and black", () => {equal(~message="Brown and black", value(["brown", "black"]), 10)})

test("Blue and grey", () => {equal(~message="Blue and grey", value(["blue", "grey"]), 68)})

test("Yellow and violet", () => {
  equal(~message="Yellow and violet", value(["yellow", "violet"]), 47)
})

test("White and red", () => {equal(~message="White and red", value(["white", "red"]), 92)})

test("Orange and orange", () => {
  equal(~message="Orange and orange", value(["orange", "orange"]), 33)
})

test("Ignore additional colors", () => {
  equal(~message="Ignore additional colors", value(["green", "brown", "orange"]), 51)
})

test("Black and brown, one-digit", () => {
  equal(~message="Black and brown, one-digit", value(["black", "brown"]), 1)
})
