class Triangle {
  construct new(a, b, c) {
    _sides = [a, b, c]
    _sides.sort()
  }

  // Is this a valid triangle?
  // - the smallest side must be positive length
  // - the longest side is shorter than the sum of the other sies.
  valid { _sides[0] > 0 && _sides[2] < _sides[0] + _sides[1] }

  // all sides equal
  isEquilateral {
    return valid && 
      _sides[0] == _sides[1] &&
      _sides[0] == _sides[2]
  }

  // any two sides equal
  isIsosceles {
    return valid && 
      _sides[0] == _sides[1] ||
      _sides[0] == _sides[2] ||
      _sides[1] == _sides[2]
  }

  // all sides different
  isScalene {
    return valid && !isIsosceles
  }
}
