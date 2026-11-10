class Line {
  construct new(x, y, angle) {
    _x = x
    _y = y
    _angle = (angle % 360) + 360 % 360
    _theta = _angle * Num.pi / 180
  }

  x { _x }
  y { _y }
  angle { _angle }
  theta { _theta }

  toString { "line: (%(x), %(y)) @ %(angle)" }

  // is the given prism "inline" with this line:
  hasPrism(prism) {
    // determine the perpendicular distance
    var dist = (prism.x - x) * theta.sin - (prism.y - y) * theta.cos
		// epsilon value determined by trial and error
    return dist.abs <= 0.0011
  }

  // given a list of prisms, find the closest one that is inline
  nextPrism(prisms) {
    var next = null
    var dist = Num.largest
    for (prism in prisms) {
      var projection = (prism.x - x) * theta.cos + (prism.y - y) * theta.sin
      if (projection > 0 && projection < dist) {
        next = prism
        dist = projection
      }
    }
    return next
  }
}
