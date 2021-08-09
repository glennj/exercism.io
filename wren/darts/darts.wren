class Darts {
  static score(x,y) {
    var dist = (x * x + y * y).sqrt

    if (dist <=  1) return 10
    if (dist <=  5) return 5
    if (dist <= 10) return 1
    return 0
  }
}
