open Test
open ResistorColorTrio

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("Orange and orange and black", () => {
  equal(~message="Orange and orange and black", label(["orange", "orange", "black"]), "33 ohms")
})

test("Blue and grey and brown", () => {
  equal(~message="Blue and grey and brown", label(["blue", "grey", "brown"]), "680 ohms")
})

test("Red and black and red", () => {
  equal(~message="Red and black and red", label(["red", "black", "red"]), "2 kiloohms")
})

test("Green and brown and orange", () => {
  equal(~message="Green and brown and orange", label(["green", "brown", "orange"]), "51 kiloohms")
})

test("Yellow and violet and yellow", () => {
  equal(
    ~message="Yellow and violet and yellow",
    label(["yellow", "violet", "yellow"]),
    "470 kiloohms",
  )
})

test("Blue and violet and blue", () => {
  equal(~message="Blue and violet and blue", label(["blue", "violet", "blue"]), "67 megaohms")
})

test("Minimum possible value", () => {
  equal(~message="Minimum possible value", label(["black", "black", "black"]), "0 ohms")
})

test("Maximum possible value", () => {
  equal(~message="Maximum possible value", label(["white", "white", "white"]), "99 gigaohms")
})

test("First two colors make an invalid octal number", () => {
  equal(
    ~message="First two colors make an invalid octal number",
    label(["black", "grey", "black"]),
    "8 ohms",
  )
})

test("Ignore extra colors", () => {
  equal(
    ~message="Ignore extra colors",
    label(["blue", "green", "yellow", "orange"]),
    "650 kiloohms",
  )
})
