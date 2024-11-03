class PrimeFactors {
  static factors(value) {
    var fs = []
    var f = 2
    while (f * f <= value) {
      while (value % f == 0) {
        fs.add(f)
        value = value / f
      }
      f = f + 1
    }
    if (value > 1) { fs.add(value) }
    return fs
  }
}
