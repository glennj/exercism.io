class NumExt {
  static isPalindrome(n) { n == reverse(n) }

  static reverse(n) {
    var m = 0
    while (n > 0) {
      m = m * 10 + n % 10
      n = (n / 10).truncate
    }
    return m
  }

  static factorsInRange(n, low, hi) {
    var fs = []
    var f = low
    while (f * f <= n && f <= hi) {
      if (n % f == 0) {
        var g = n / f
        if (g < f) break
        if (g <= hi) fs.add([f, g])
      }
      f = f + 1
    }
    return fs
  }
}
