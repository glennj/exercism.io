import "wren-testie/testie" for Testie, Expect
import "./resistor-color" for Resistor, COLORS

Testie.test("Resistor Color") { |do, skip|
  do.describe("Color codes") {
    do.test("Black") {
      Expect.value(Resistor.colorCode("black")).toEqual(0)
    }

    do.test("White") {
      Expect.value(Resistor.colorCode("white")).toEqual(9)
    }

    do.test("Orange") {
      Expect.value(Resistor.colorCode("orange")).toEqual(3)
    }
  }

  do.test("Colors") {
    Expect.value(COLORS).toEqual([
      "black",
      "brown",
      "red",
      "orange",
      "yellow",
      "green",
      "blue",
      "violet",
      "grey",
      "white",
    ])
  }
}
