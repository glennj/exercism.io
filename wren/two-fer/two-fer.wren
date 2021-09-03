class TwoFer {
  static twoFer() {
    return twoFer("you")
  }

  // A single expression method body, no `return` required.
  // This uses String interpolation.
  static twoFer(name) { "One for %(name), one for me." }
}
