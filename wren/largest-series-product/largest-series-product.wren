class Series {
  construct new(digitString, span) {
    // `48` is the codePoint of character '0'
    _digits = digitString.codePoints.map {|c| c - 48}.toList
    _span = span

    assert_(_span >= 0, "span must not be negative")
    assert_(_span <= _digits.count, "span must not exceed string length")
    assert_(_digits.all {|d| 0 <= d && d <= 9}, "digits input must only contain digits")
  }

  largestProduct {
    return (0..(_digits.count - _span)).reduce(0) {|max, i|
      var product = _digits[i...(i + _span)].reduce(1) {|prod, d| prod * d}
      return product.max(max)
    }
  }

  assert_(cond, msg) {
    if (!cond) Fiber.abort(msg)
  }
}
