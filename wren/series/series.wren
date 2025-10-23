import "./assert" for Assert

class Series {
  construct new(digits) {
    Assert.refute(digits.isEmpty, "series cannot be empty")

    _str = digits
    _len = digits.count
  }

  slices(size) {
    Assert.assert(size <= _len, "slice length cannot be greater than series length")
    Assert.assert(size != 0, "slice length cannot be zero")
    Assert.assert(size >= 0, "slice length cannot be negative")

    return (0.._len - size).map { |i| _str.skip(i).take(size).join() }.toList
  }
}
