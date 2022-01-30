var makeLabel = Fn.new { |opts|
  return "Resistor value: %(opts["value"]) %(opts["unit"])"
}

import "./resistor-color-trio" for ResistorColorTrio
import "wren-testie/testie" for Testie, Expect

Testie.test("Resistor Color Trio") { |do, skip|
  do.test("Orange and orange and black") {
    Expect.value(ResistorColorTrio.new(["orange", "orange", "black"]).label).toEqual(
      makeLabel.call({ "value": 33, "unit": "ohms" })
    )
  }

  do.test("Blue and grey and brown") {
    Expect.value(ResistorColorTrio.new(["blue", "grey", "brown"]).label).toEqual(
      makeLabel.call({ "value": 680, "unit": "ohms" })
    )
  }

  do.test("Red and black and red") {
    Expect.value(ResistorColorTrio.new(["red", "black", "red"]).label).toEqual(
      makeLabel.call({ "value": 2, "unit": "kiloohms" })
    )
  }

  do.test("Green and brown and orange") {
    Expect.value(ResistorColorTrio.new(["green", "brown", "orange"]).label).toEqual(
      makeLabel.call({ "value": 51, "unit": "kiloohms" })
    )
  }

  do.test("Yellow and violet and yellow") {
    Expect.value(ResistorColorTrio.new(["yellow", "violet", "yellow"]).label).toEqual(
      makeLabel.call({ "value": 470, "unit": "kiloohms" })
    )
  }

  // optional: error
  do.test("Invalid color") {
    Expect.that {
      ResistorColorTrio.new(["yellow", "purple", "black"]).label
    }.abortsWith("invalid color")
  }
}
