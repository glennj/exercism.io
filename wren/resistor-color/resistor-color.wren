// Better Be Right Or Your Great Big Values Go Wrong
var COLORS = [
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
]

class Resistor {
  /* Return the resistor value code for a colour.
     Will return -1 for an invalid/unknown colour.
  */
  static colorCode(color) { COLORS.indexOf(color) }
}
