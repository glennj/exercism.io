import "./line" for Line

class Prism {
  static findSequence(prisms, start) {
    var line = Line.new(start["x"], start["y"], start["angle"])
    prisms = prisms.map {|p| this.new(p["id"], p["x"], p["y"], p["angle"])}
    
    var seq = []
    while (true) {
      var inline = prisms.where {|p| line.hasPrism(p) }
      var p = line.nextPrism(inline)

      if (!p) break

      seq.add(p.id)
      line = Line.new(p.x, p.y, line.angle + p.angle)
    }
    return seq
  }

  construct new(id, x, y, angle) {
    _id = id
    _x = x
    _y = y
    _angle = angle
  }

  x { _x }
  y { _y }
  id { _id }
  angle { _angle }

  toString { "prism %(id): (%(x), %(y)) @ %(angle)" }
}
