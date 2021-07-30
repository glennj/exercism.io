class TwoFer {
  static twoFer() {
    return twoFer("you")
  }

  // a single expression method body, no `return` required.
  // with String interpolation.
  static twoFer(name) { "One for %(name), one for me." }
}
