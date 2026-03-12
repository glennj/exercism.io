class TwoFer {
  // Taking advantage of method overloading: https://wren.io/method-calls.html#signature
  static twoFer() {
    return twoFer("you")
  }

  // A single expression method body, no `return` required.
  // This uses String interpolation.
  static twoFer(name) { "One for %(name), one for me." }
}
