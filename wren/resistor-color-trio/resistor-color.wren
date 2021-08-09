var COLORS = [
  "black", "brown", "red", "orange", "yellow",
  "green", "blue", "violet", "grey", "white",
]

class Resistor {
  static colorCode(color) { 
    var code = COLORS.indexOf(color) 
    if (code == -1) Fiber.abort("invalid color")
    return code
  }
}
