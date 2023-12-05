class PythagoreanTriplet {
  static tripletsWithSum(n) {
    var triplets = []
    var a = 2
    while (true) {
      a = a + 1
      var b = n * (n - 2*a) / (2 * (n - a))
      if (a >= b) break
      if (b.isInteger) {
        var c = n - a - b
        triplets.add([a, b, c])
      }
    }
    return triplets
  }
}
